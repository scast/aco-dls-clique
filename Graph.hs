{-# LANGUAGE BangPatterns #-}
module Graph (createGraph, connected, connectedAll, disconnectedOne, nodeCount,
              improvementSet, levelSet, Set, Graph) where
import Data.Bits
import Data.Foldable (all, Foldable, foldl')
import Control.Monad
import Prelude hiding (all)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.BitSet.Dynamic (FasterInteger)


type Matrix = V.Vector FasterInteger
type Set = FasterInteger
data Graph = Graph { edgeCount :: !Int,
                     nodeCount :: !Int,
                     matrix :: !Matrix,
                     degree :: !(V.Vector Int) }
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
                           edgeCount = length xs, nodeCount = V.length matrix }
  where matrix = createGraph' n xs
        degrees = V.generate n (\x -> popCount (matrix V.! x))

-- | Checks if `x` is connected to `y`
connected :: Graph -> Int -> Int -> Bool
connected !g x y = (m V.! x) `testBit` y
  where m = matrix g

-- | Checks if a given node `x` is connected to all nodes in `cc`
connectedAll :: Graph -> Int -> Set -> Bool
connectedAll !g x cc = ((m V.! x) .&. cc) == cc
  where m = matrix g

swapWith bm = snd $ until ((flip testBit 0) . fst) go (bm, 0)
  where go (!nbm, !cnt) = (nbm `shiftR` 1, cnt+1)

-- | Returns the element to `x` with in `cc`.
disconnectedOne g x cc = swapWith (bm `xor` cc)
  where m = matrix g
        bm = ((m V.! x) .&. cc)

-- | Returns whether or not `x` is connected to *all* nodes except for
-- *one* in `cc`
isDisconnectedFromOne g !x !cc !bc = popCount bm == bc - 1
  where m = matrix g
        bm = (m V.! x) .&. cc

-- | The vertex set of possible expansions for a clique `cc`
improvementSet :: Graph -> Set -> Set -> [Int]
improvementSet g cc alreadyUsed = foldl' go [] notClique
  where
    notClique = map cond [0..(nodeCount g)-1]
    cond x = (x, (not (alreadyUsed `testBit` x)) && (not (cc `testBit` x)) && (connectedAll g x cc))
    go !acum !(pos, True) = pos:acum
    go !acum !(_, False) = acum

-- | The vertex set of possible swaps for a clique `cc`
levelSet :: Graph -> Set -> Set -> [Int]
levelSet g cc alreadyUsed = foldl' go [] notClique
  where
    notClique = map cond [0..(nodeCount g)-1]
    !bc = popCount cc
    cond !x = (x, not (cc `testBit` x) && not (alreadyUsed `testBit` x) && isDisconnectedFromOne g x cc bc)
    go !acum !(pos, True) = pos:acum
    go !acum !(_, False) = acum
