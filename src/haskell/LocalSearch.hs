{-# LANGUAGE PackageImports #-}
module LocalSearch (Settings(..), DLS(..), goDLS) where
import Data.Bits
import Data.Maybe (fromJust, isJust, maybe)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.Vector.Algorithms.Intro (sort)
import qualified Data.ByteString.Lazy as B (readFile)
import qualified Data.Foldable as DF
import qualified Data.IntMap as DM
import Control.Monad.Loops
import System.Random
import System.Environment
import System.Exit
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad (forM)
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Reader

import Graph
import MyParser

type PenaltyMap = V.Vector (Int, Int)

data EvalState = EvalState { _currentClique :: !Set
                           , bestClique :: !Set
                           , numSteps ::  !Int
                           , penalty :: !PenaltyMap
                           , lastAdded :: !Int
                           , updateCycle :: !Int
                           , alreadyUsed :: !Set
                           , currentPosition :: !Int }

data Settings = Settings { graph :: !Graph,
                           maxSteps :: !Int,
                           sharedPenalties :: !(MVar (PenaltyMap)),
                           cliqueChan :: !(Chan (Set)),
                           penaltyDelay :: !Int }

type CliqueState s a = ReaderT Settings (StateT s IO) a

class SelectCriteria s where
  -- | How to select a node to be added to the current clique.
  selectToExpand :: CliqueState s (Maybe Int)

  -- | How to select a node to be swapped out of the current clique.
  selectToSwap :: CliqueState s (Maybe Int)

  -- | Decide if the improving set is empty.
  canImprove :: CliqueState s Bool

class SelectCriteria s => DLS s where
  -- | Compute the initial state of the search
  getInitial :: Graph -> IO s

  -- | Add a vertex to the current clique
  addVertex :: Int -> CliqueState s ()

  -- | Swap out a vertex from the current clique
  swapVertex :: Int -> CliqueState s ()

  -- | Update the best answer found so far.
  updateBest :: CliqueState s ()

  -- | Update the current state just before starting a new cycle
  update :: CliqueState s ()

  -- | How to restart the search on the next cycle
  restart :: CliqueState s ()

  -- | The stopping criteria for the main algorithm
  stopCriteria :: CliqueState s Bool

  -- | Return the currentClique found.
  currentClique :: CliqueState s Set

-- | SelectionCriteria based on minimizing the penalty value of the
-- selected node
instance SelectCriteria EvalState where
  selectToExpand = do
    cc <- gets _currentClique
    au <- gets alreadyUsed
    pm <- gets penalty
    pos <- gets currentPosition
    g <- asks graph
    case improvementSet g cc au pm pos of
      Just (next, pos) -> do modify (\st -> st { currentPosition = pos })
                             return $ Just next
      Nothing -> modify (\st -> st { currentPosition = 0 }) >> return Nothing

  selectToSwap = do
    cc <- gets _currentClique
    au <- gets alreadyUsed
    pm <- gets penalty
    g <- asks graph
    return $ levelSet g cc au pm

  canImprove = do
    next <- selectToExpand
    maybe (return False) (\_ -> do modify (\st -> st { currentPosition = 0 })
                                   return True) next


instance DLS EvalState where
  getInitial g = do
    let n = nodeCount g
    initialVertex <- randomRIO (0, n-1)
    return EvalState { _currentClique = 0 `setBit` initialVertex
                     , bestClique = 0 `setBit` initialVertex
                     , numSteps = 0
                     , lastAdded = initialVertex
                     , penalty = V.generate n $ \x -> (0, x)
                     , updateCycle = 1
                     , alreadyUsed = 0
                     , currentPosition = 0}

  addVertex v = modify (\st -> st { _currentClique = (_currentClique st) `setBit` v
                                  , alreadyUsed = (alreadyUsed st) `setBit` v
                                  , lastAdded = v
                                  , numSteps = succ $ numSteps st })

  swapVertex v = do
    g <- asks graph
    modify (\st ->
             let remove = disconnectedOne g v (_currentClique st)
                 newClique = (_currentClique st) `setBit` v `clearBit` remove
                 newAlreadyUsed = (alreadyUsed st) `setBit` v
             in st { _currentClique = newClique
                   , lastAdded = v
                   , alreadyUsed = newAlreadyUsed
                   , numSteps = succ $ numSteps st})

  updateBest = do
    bc <- gets bestClique
    cc <- currentClique
    if popCount bc < popCount cc
      then do chan <- asks cliqueChan
              liftIO $ writeChan chan cc
              modify (\st -> st { bestClique = cc })
      else return ()

  update = do
    mvPM <- asks sharedPenalties
    pd <- asks penaltyDelay
    uc <- gets updateCycle
    cc <- currentClique
    pm <- gets penalty
    g <- asks graph
    let dec = if uc `mod` pd == 0 then -1 else 0
    liftIO $ modifyMVar_ mvPM (updatePenalties g cc dec)
    pm <- liftIO $ readMVar mvPM
    modify (\st -> st { updateCycle = succ uc
                      , penalty = pm })


  stopCriteria = do
    ns <- gets numSteps
    ms <- asks maxSteps
    return (ns < ms)

  currentClique = gets _currentClique

  restart = do
    pd <- asks penaltyDelay
    if pd > 1
      then do v <- gets lastAdded
              modify $ \st -> st { _currentClique = 0 `setBit` v
                                 , alreadyUsed = 0}
      else do g <- asks graph
              let n = nodeCount g
              v <- liftIO $ randomRIO (0, n-1)
              cc' <- liftM (flip setBit v) currentClique
              let newClique = DF.foldl' (\acum i -> if i /= v && cc' `testBit` i &&
                                                       not (connected g i v)
                                                    then acum `clearBit` i
                                                    else acum) cc' [0..n-1]
              modify $ \st -> st { _currentClique = newClique
                                 , lastAdded = v
                                 , alreadyUsed = 0 }

-- | Dynamic Local Search (DLS)
dls :: DLS s => CliqueState s ()
dls = whileM_ stopCriteria $ do
  expand
  c' <- currentClique
  plateau c'
  phases
  update
  restart

-- | Decide if the level set is empty. If not, swaps a vertex
-- according to `s` criteria and returns True.
canSwap :: DLS s => CliqueState s Bool
canSwap = do
  next <- selectToSwap
  maybe (return False) (\v -> swapVertex v >> return True) next

-- | Decide if the current clique and the stored clique overlap.
cliquesOverlap :: DLS s => Set -> CliqueState s Bool
cliquesOverlap c' = do
  cc <- currentClique
  return (cc .&. c' /= 0)

-- | Expand the current clique
expand :: DLS s => CliqueState s ()
expand = whileJust_ selectToExpand  addVertex

-- | Swap nodes from the current clique
plateau :: DLS s => Set -> CliqueState s ()
plateau c' =
  whileM_ (andM [cliquesOverlap c', liftM not canImprove, canSwap]) $ return ()

-- | Phases of expand and plateau search
phases :: DLS s => CliqueState s ()
phases = whileM_ canImprove $ do
  expand
  cc <- currentClique
  plateau cc

updatePenalties :: Monad m => Graph -> Set -> Int -> PenaltyMap
                   -> m (V.Vector (Int, Int))
updatePenalties g cc dec pm = return $ V.create $ do
  let up = V.generate (nodeCount g) $ \x ->
        (max 0 (fst (pm V.! x) + dec +
               if cc `testBit` (snd (pm V.! x)) then 1 else 0)
         , snd (pm V.! x))
  v <- V.thaw up
  sort v
  return v

-- goDLS ::
goDLS graph settings = getInitial graph >>= runStateT (runReaderT
                                                       (dls :: CliqueState EvalState ())
                                                       settings)
