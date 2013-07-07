{-# LANGUAGE PackageImports #-}
-- module LocalSearch (setInitial) where
import Data.Bits
import Data.Maybe (fromJust)
import Control.Monad (forM)
import System.Random
import System.Environment
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import qualified Data.ByteString.Lazy as B (readFile)
import qualified Data.Foldable as DF
import qualified Data.IntMap as DM
import "mtl" Control.Monad.State

import Graph
import Parser

type PenaltyMap = (DM.IntMap (Int, Int))

data EvalState = EvalState { graph :: !Graph,
                             currentClique :: !Set,
                             bestClique :: !(MVar Set),
                             numSteps :: !(MVar Int),
                             penaltyDelay :: !Int,
                             penalty :: !(MVar PenaltyMap),
                             lastAdded :: !(Maybe Int),
                             updateCycle :: !Int,
                             alreadyUsed :: !Set,
                             foundCliques :: !(Chan Set)}

type SharedState = (MVar PenaltyMap, MVar Int, MVar Set, Chan Set)
type CliqueState a = StateT EvalState IO a

-- | Builds the shared state across our threads
getSharedState :: Int -> IO (SharedState)
getSharedState n = do
  penaltySTM <- newMVar (DM.fromList [(x, (0, x)) | x <- [0..n-1]])
  numSteps <- newMVar 0
  bestClique <- newMVar 0
  foundCliques <- newChan
  return (penaltySTM, numSteps, bestClique, foundCliques)

-- | Get the initial state for a thread.
getInitial :: Graph -> Int -> SharedState -> IO (EvalState)
getInitial g pd (penaltySTM, numSteps, bestClique, foundCliques) = do
  n <- return (nodeCount g)
  initialVertex <- randomRIO (0, n-1)
  return EvalState {graph = g,
                    currentClique = 0 `setBit` initialVertex,
                    bestClique = bestClique,
                    numSteps = numSteps,
                    lastAdded = Nothing,
                    penaltyDelay = pd,
                    penalty = penaltySTM,
                    updateCycle = 1,
                    alreadyUsed = 0,
                    foundCliques = foundCliques}

-- | Dynamic Local Search for a Maximum Clique
dls :: Int -> CliqueState Set
dls maxSteps = do
  st <- get
  ns <- liftIO $ readMVar (numSteps st)
  if (ns < maxSteps) then
    do
      -- liftIO $ putStrLn ("Paso -> " ++ (show ns))
      expand
      plateau (currentClique st)
      phases
      updatePenalties
      restart
      dls maxSteps
  else liftIO (readMVar (bestClique st))

inc :: Int -> IO (Int)
inc = return . succ

-- | Expand the current clique
expand :: CliqueState ()
expand = do
  st <- get
  let au = alreadyUsed st
  is <- return $ improvementSet (graph st) (currentClique st) au
  if (null is)
    then updateBest
    else do v <- selectMinPenalty is
            newClique <- return $ (currentClique st) `setBit` v
            liftIO $ modifyMVar_ (numSteps st) inc
            put st {currentClique = newClique, lastAdded = Just v,
                    alreadyUsed = (alreadyUsed st) `setBit` v}
            expand

-- | Swap nodes from the current clique
plateau :: Set -> CliqueState ()
plateau c' = do
  st <- get
  let au = alreadyUsed st
  ls <- return $ levelSet (graph st) (currentClique st) au
  is <- return $ improvementSet (graph st) (currentClique st) au
  if and [not (null ls), ((currentClique st) .&. c') /= 0 , null is]
    then do v <- selectMinPenalty ls
            let remove = (disconnectedOne (graph st) v (currentClique st))
            newClique <- return $ (currentClique st) `setBit` v `clearBit` remove
            liftIO $ modifyMVar_ (numSteps st) inc
            put st {currentClique = newClique, lastAdded = Just v,
                    alreadyUsed = (alreadyUsed st) `setBit` v}
            plateau c'
    else return ()


-- | Update the best clique found so far.
updateBest :: CliqueState ()
updateBest = do
  st <- get
  liftIO $ modifyMVar_ (bestClique st) $ \bc -> do
    if popCount bc < popCount (currentClique st)
      then do writeChan (foundCliques st) (currentClique st)
              return (currentClique st)
      else return bc

-- | Select the node with minimum penalty.
selectMinPenalty :: [Int] -> CliqueState Int
selectMinPenalty set = do
  st <- get
  penalties <- liftIO $ readMVar (penalty st)
  let ans = minimum (map (\x -> (fromJust (DM.lookup x penalties))) set)
  return (snd ans)

-- | Heuristic search mixing penalties and vertex degree
selectBestHeuristic :: [Int] -> CliqueState Int
selectBestHeuristic set = do
  st <- get
  penalties <- liftIO $ readMVar (penalty st)
  let dg = degree (graph st)
  let maxi = maxDegree (graph st)
  let ans = DF.minimum (Prelude.map (\x -> (fromJust (DM.lookup x penalties))*(maxi - (dg V.! x))) set)
  return (snd ans)

-- | Phases of expand and plateau search
phases :: CliqueState ()
phases = do
  st <- get
  let au = alreadyUsed st
  is <- return $ improvementSet (graph st) (currentClique st) au
  if (null is)
    then return ()
    else do expand
            plateau (currentClique st)
            phases

-- | Update clique penalties.
updatePenalties :: CliqueState ()
updatePenalties = do
  st <- get
  let dec = if ((updateCycle st) `mod` (penaltyDelay st)) == 0 then -1 else 0
  let cc = currentClique st
  liftIO $ modifyMVar_ (penalty st) $ \penalties ->
    return ((DF.foldl' (go cc dec) penalties
             [0..(nodeCount (graph st))-1]))
  put st {updateCycle = (updateCycle st) + 1}
  where go cc dec penaltyMap node = DM.adjustWithKey (modifyPenalty cc dec)
                                    node penaltyMap
        modifyPenalty cc dec key val = (min 0 ((fst val) + dec
                                               + (if cc `testBit` key then 1 else 0)), snd val)

-- | Restart the search randomly.
restart :: CliqueState ()
restart = do
  st <- get
  put st {currentClique = 0 `setBit` (fromJust (lastAdded st)),
          alreadyUsed = 0}

main :: IO ()
main = do
  [filename] <- getArgs
  file <- B.readFile filename
  case parseByteString "" file of
    Right (GraphEdges n _ e) ->
      do
        shared@(_, _, _, chan) <- getSharedState n
        let graph = createGraph n e
        forM [1..4] $ \x -> do
          forkIO $ do
            initialState <- getInitial graph 1 shared
            runStateT (dls 15000000) initialState
            putStrLn "Listo."
        cliques <- getChanContents chan
        forM cliques $ \clique -> do
          putStrLn ("Se consiguio un clique de tamano " ++(show (popCount clique)))
        putStrLn "Muerete que chao."
    Left e -> error "Nope."
