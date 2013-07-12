{-# LANGUAGE BangPatterns, TypeOperators #-}
module Graph ( createGraph
             , connected
             , connectedAll
             , disconnectedOne
             , improvementSet
             , levelSet
             , updateImprovementSet
             , updateImprovementSetS
             , setToList
             , Set
             , Graph(..)
             ) where
import Data.Bits
import Data.Foldable (all, Foldable, foldl', maximum)
import Control.Monad
import Prelude hiding (all)
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
createGraph' !n !xs = V.create $ do
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
        degrees = V.generate n (\x -> popCount (matrix V.! x))
        mDegree = V.maximum degrees

-- | Checks if `x` is connected to `y`
connected :: Graph -> Int -> Int -> Bool
connected !g x y = (m V.! x) `testBit` y
  where m = matrix g

-- | Checks if a given node `x` is connected to all nodes in `cc`
connectedAll :: Graph -> Int -> Set -> Bool
connectedAll !g x cc = ((m V.! x) .&. cc) == cc
  where m = matrix g

naiveSwapWith bm = snd $ until ((flip testBit 0) . fst) go (bm, 0)
  where go (!nbm, !cnt) = (nbm `shiftR` 1, cnt+1)

-- | Binary searches the first set bit.
swapWith :: (Num a, Bits a) => a -> Int -> Int -> Int
swapWith bm lo hi = let mid = (lo+hi) `div` 2
                    in if (bm `testBit` mid)
                       then mid
                       else if (bm .&. ((0 `setBit` mid) -1)) == 0
                            then swapWith bm (mid+1) hi
                            else swapWith bm lo (mid-1)

-- | Returns the element to swap `x` with in `cc`.
disconnectedOne g x cc =  swapWith (bm `xor` cc) 0 (n-1)
  where m = matrix g
        n = nodeCount g
        bm = ((m V.! x) .&. cc)

-- | Returns whether or not `x` is connected to *all* nodes except for
-- *one* in `cc`
isDisconnectedFromOne g !x !cc !bc = popCount bm == bc - 1
  where m = matrix g
        bm = (m V.! x) .&. cc

-- | The vertex set of possible expansions for a clique `cc`
improvementSet :: Graph -> Set -> Set -> [Int]
improvementSet g cc alreadyUsed = filter cond [0..(nodeCount g)-1]
  where
    cond x = (not (alreadyUsed `testBit` x)) && (not (cc `testBit` x)) && (connectedAll g x cc)

-- | Update the improvement set after expanding the clique with node v
updateImprovementSet :: Graph -> [Int] -> Int -> Set -> [Int]
updateImprovementSet g i0 v alreadyUsed = filter cond i0
  where cond y = (connected g v y) && not (alreadyUsed `testBit` y)

-- | Update the improvement set after swapping a vertex `x` for a vertex `y`
updateImprovementSetS :: Graph -> Set -> [Int] -> Int -> Int -> Set -> [Int]
updateImprovementSetS g cc0 l0 x v alreadyUsed = filter cond l0
  where cond y = (y /= v) && ((disconnectedOne g y cc0) == x) &&
                 (connected g y v) && not (alreadyUsed `testBit` y)

-- | The vertex set of possible swaps for a clique `cc`
levelSet :: Graph -> Set -> Set -> [Int]
levelSet g cc alreadyUsed = filter cond [0..(nodeCount g)-1]
  where
    !bc = popCount cc
    cond !x = not (cc `testBit` x) && not (alreadyUsed `testBit` x) && isDisconnectedFromOne g x cc bc
