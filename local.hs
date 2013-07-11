{-# LANGUAGE PackageImports #-}
-- module LocalSearch (setInitial) where
import Data.Bits
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as B (readFile)
import qualified Data.Foldable as DF
import qualified Data.IntMap as DM
import System.Random
import System.Environment
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad (forM)
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Reader

import Graph
import Parser

type PenaltyMap = (DM.IntMap (Int, Int))

data EvalState = EvalState { currentClique :: Set,
                             bestClique :: Set,
                             numSteps ::  Int,
                             penalty :: PenaltyMap,
                             lastAdded :: Int,
                             updateCycle :: Int,
                             alreadyUsed :: Set,
                             is :: [Int] }

data Settings = Settings { graph :: Graph,
                           maxSteps :: Int,
                           sharedPenalties :: MVar (PenaltyMap),
                           cliqueChan :: Chan (Set),
                           penaltyDelay :: Int}

type SharedState = (MVar PenaltyMap, Chan Set)
type MyStateMonad s a = StateT s IO a
type CliqueState a = ReaderT Settings (StateT EvalState IO) a

-- | Builds the shared state across our threads
getSharedState :: Int -> IO (SharedState)
getSharedState n = do
  penaltySTM <- newMVar (DM.fromList [(x, (0, x)) | x <- [0..n-1]])
  foundCliques <- newChan
  return (penaltySTM, foundCliques)

-- | Get the initial state for a thread.
getInitial :: Graph -> IO (EvalState)
getInitial g = do
  let n = nodeCount g
  initialVertex <- randomRIO (0, n-1)
  return EvalState { currentClique = 0 `setBit` initialVertex,
                     bestClique = 0,
                     numSteps = 0,
                     lastAdded = initialVertex,
                     penalty = DM.empty,
                     updateCycle = 1,
                     alreadyUsed = 0,
                     is = [] }

-- | Dynamic Local Search for a Maximum Clique
dls :: CliqueState ()
dls = do
  settings <- ask
  st <- get
  let ns = numSteps st
      ms = maxSteps settings
      mvPM = sharedPenalties settings
  if (ns < ms) then
    do
      -- We must fully calculate the improvement set ONCE.
      put st {is = improvementSet (graph settings) (currentClique st) (alreadyUsed st) }
      -- Then we must retrieve the current penalty map for this iteration
      pm <- liftIO $ readMVar mvPM
      put st { penalty = pm }
      -- We do a single expand/plateau phase
      expand
      plateau (currentClique st)
      -- Expand/Plateau until no more
      phases
      -- Update global penalties
      updatePenalties
      restart
      dls
  else return ()

inc :: Int -> IO (Int)
inc = return . succ

-- | Expand the current clique
expand :: CliqueState ()
expand = do
  st <- get
  settings <- ask
  let mis = is st
  if (null mis)
    then updateBest
    else do v <- selectMinPenalty mis
            let newClique = (currentClique st) `setBit` v
                newAlreadyUsed = (alreadyUsed st) `setBit` v
            put st {currentClique = newClique, lastAdded = v,
                    alreadyUsed = newAlreadyUsed, numSteps = (numSteps st) + 1,
                    is = updateImprovementSet (graph settings) mis v newAlreadyUsed}
            expand

-- | Swap nodes from the current clique
plateau :: Set -> CliqueState ()
plateau c' = do
  st <- get
  settings <- ask
  let au = alreadyUsed st
  let ls = levelSet (graph settings) (currentClique st) au
  if and [((currentClique st) .&. c') /= 0, not (null ls)]
    then do v <- selectMinPenalty ls
            let remove = (disconnectedOne (graph settings) v (currentClique st))
                newClique = (currentClique st) `setBit` v `clearBit` remove
                newAlreadyUsed = (alreadyUsed st) `setBit` v
                oldClique = (currentClique st)
            put st {currentClique = newClique, lastAdded =  v,
                    alreadyUsed = newAlreadyUsed, numSteps = (numSteps st) + 1,
                    is = updateImprovementSetS (graph settings) oldClique ls remove v newAlreadyUsed}
            let newIs = is st
            if null newIs
              then plateau c'
              else return ()
    else return ()


-- | Update the best clique found so far.
updateBest :: CliqueState ()
updateBest = do
  st <- get
  let bc = bestClique st
      cc = currentClique st
  if popCount bc < popCount cc
    then do settings <- ask
            let chan = cliqueChan settings
            liftIO $ writeChan chan cc
            put st { bestClique = cc }
    else return ()

-- | Select the node with minimum penalty.
selectMinPenalty :: [Int] -> CliqueState Int
selectMinPenalty set = do
  st <- get
  let penalties = (penalty st)
  let ans = minimum (map (\x -> (fromJust (DM.lookup x penalties))) set)
  return (snd ans)

-- -- | Heuristic search mixing penalties and node degree
-- selectBestHeuristic :: [Int] -> CliqueState Int
-- selectBestHeuristic set = do
--   st <- get
--   pe <- liftIO $ readMVar (penalty st)
--   let dg = degree (graph st)
--   let maxi = maxDegree (graph st)
--   let ans = DF.minimum (lOrd maxi dg pe)
--   return (snd ans)
--   where
--     lOrd maxi dg pe = Prelude.map (\(val,pos) -> ( val*(maxi-(dg V.! pos)) ,pos)) (lPen pe)
--     lPen pe = Prelude.map (\x -> (fromJust (DM.lookup x pe))) set

-- | Phases of expand and plateau search
phases :: CliqueState ()
phases = do
  st <- get
  let au = alreadyUsed st
      mis = is st
  if (null mis)
    then return ()
    else do expand
            plateau (currentClique st)
            phases

-- | Update clique penalties.
updatePenalties :: CliqueState ()
updatePenalties = do
  st <- get
  settings <- ask
  let mvPM = sharedPenalties settings
      dec = if ((updateCycle st) `mod` (penaltyDelay settings)) == 0 then -1 else 0
      cc = currentClique st
  liftIO $ modifyMVar_ mvPM $ \penalties ->
    return ((DF.foldl' (go cc dec) penalties
             [0..(nodeCount (graph settings))-1]))
  put st {updateCycle = (updateCycle st) + 1}
  where go cc dec penaltyMap node = DM.adjustWithKey (modifyPenalty cc dec)
                                    node penaltyMap
        modifyPenalty cc dec key val = (min 0 ((fst val) + dec
                                               + (if cc `testBit` key then 1 else 0)), snd val)

-- | Restart the search randomly.
restart :: CliqueState ()
restart = do
  st <- get
  put st {currentClique = 0 `setBit` (lastAdded st),
          alreadyUsed = 0}

goDLS graph settings = getInitial graph >>= runStateT (runReaderT dls settings)

main :: IO ()
main = do
  [filename, spd, steps] <- getArgs
  file <- B.readFile filename
  case parseByteString "" file of
    Right (GraphEdges n _ e) ->
      do
        shared@(mvPM, chan) <- getSharedState n
        let graph = createGraph n e
        forM [1..4] $ \x -> do
          forkIO $ do
            goDLS graph Settings { graph = graph,
                                   maxSteps = read steps,
                                   penaltyDelay = read spd,
                                   sharedPenalties = mvPM,
                                   cliqueChan = chan}
            putStrLn "Listo."
        cliques <- getChanContents chan
        forM cliques $ \clique -> do
          putStrLn ("Se consiguio un clique de tamano " ++(show (popCount clique)))
        putStrLn "Muerete que chao."
    Left e -> error "Nope."
