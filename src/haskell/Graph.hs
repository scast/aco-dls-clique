{-# LANGUAGE BangPatterns, TypeOperators #-}
module Graph ( createGraph
             , connected
             , connectedAll
             , disconnectedOne
             , improvementSet
             , levelSet
             , setToList
             , Set
             , Graph(..)
             ) where
import Data.Bits
import Data.Foldable (all, Foldable, foldl', maximum)
import Control.Monad
import Prelude hiding (all)
import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.BitSet.Dynamic (FasterInteger)
import Data.MemoTrie

type Matrix = V.Vector FasterInteger
type Set = FasterInteger

setToList :: Int -> Set -> [Int]
setToList n xs = filter (testBit xs) [0..(n-1)]

data Graph = Graph { edgeCount :: !Int,
                     nodeCount :: !Int,
                     matrix :: !Matrix,
                     degree :: !(V.Vector Int),
                     maxDegree :: !Int }
             deriving (Show)

createGraph' :: Int -> [(Int, Int)] -> Matrix
createGraph' !n xs = V.create $ do
  v <- VM.replicate n 0
  forM xs $ \(a, b) -> do
    valueA <- VM.read v (a-1)
    valueB <- VM.read v (b-1)
    VM.write v (a-1) (valueA `setBit` (b-1))
    VM.write v (b-1) (valueB `setBit` (a-1))
  return v

-- | Creates a graph with `n` nodes using the edges in `xs`
createGraph :: Int -> [(Int, Int)] -> Graph
createGraph !n !xs = Graph { matrix = matrix, degree = degrees,
                           edgeCount = length xs, nodeCount = V.length matrix,
                           maxDegree = mDegree}
  where matrix = createGraph' n xs
        degrees = V.generate n (\x -> popCount (matrix ! x))
        mDegree = V.maximum degrees

-- | Checks if `x` is connected to `y`
connected :: Graph -> Int -> Int -> Bool
connected !g !x !y = (m ! x) `testBit` y
  where m = matrix g

-- | Checks if a given node `x` is connected to all nodes in `cc`
connectedAll :: Graph -> Int -> Set -> Bool
connectedAll !g !x !cc = m ! x .&. cc == cc
  where m = matrix g

naiveSwapWith bm = snd $ until ((flip testBit 0) . fst) go (bm, 0)
  where go (!nbm, !cnt) = (nbm `shiftR` 1, cnt+1)

-- | Binary searches the first set bit.
swapWith :: (Num a, Bits a) => a -> Int -> Int -> Int
swapWith !bm !lo !hi = let mid = (lo+hi) `div` 2
                    in if (bm `testBit` mid)
                       then mid
                       else if bm .&. ((0 `setBit` mid) -1) == 0
                            then swapWith bm (mid+1) hi
                            else swapWith bm lo (mid-1)

-- | Returns the element to swap `x` with in `cc`.
disconnectedOne !g !x !cc =  swapWith (bm `xor` cc) 0 (n-1)
  where m = matrix g
        n = nodeCount g
        bm = m ! x .&. cc

-- | Returns whether or not `x` is connected to *all* nodes except for
-- *one* in `cc`
isDisconnectedFromOne !g !x !cc !bc = popCount bm == bc - 1
  where m = matrix g
        bm = m ! x .&. cc

-- | The vertex set of possible expansions for a clique `cc`
improvementSet :: Graph -> Set -> Set -> V.Vector (Int, Int) -> Int -> Maybe (Int, Int)
improvementSet !g !cc !alreadyUsed !pm !starting =
  let n = nodeCount g
      f x = (not (alreadyUsed `testBit` x))
            && (not (cc `testBit` x))
            && (connectedAll g x cc)
      val x = snd  (pm ! x)
      pos = val.head
      possibilities = filter(f.val) [starting..n-1]
  in if null possibilities
     then Nothing
     else Just $ (pos possibilities, succ $ head possibilities)


-- | The vertex set of possible swaps for a clique `cc`
levelSet :: Graph -> Set -> Set -> V.Vector (Int, Int) -> Maybe Int
levelSet g cc alreadyUsed pm =
  let n = nodeCount g
      bc = popCount cc
      val x = snd (pm ! x)
      cond !x = not (cc `testBit` x) &&
                not (alreadyUsed `testBit` x) &&
                isDisconnectedFromOne g x cc bc
      possibilities = filter (cond.val) [0..n-1]
  in if null possibilities
     then Nothing
     else Just $ val (head possibilities)
