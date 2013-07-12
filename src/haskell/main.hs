module Main (main) where
import qualified Data.ByteString.Lazy as B (readFile)
import qualified Data.IntMap as DM
import Data.Bits
import Control.Monad (forM)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import System.Exit
import System.Environment

import Graph
import MyParser
import LocalSearch

type SharedState = (MVar PenaltyMap, Chan Set)
type PenaltyMap = (DM.IntMap (Int, Int))

-- | Builds the shared state across our threads
getSharedState :: Int -> IO (SharedState)
getSharedState n = do
  penaltySTM <- newMVar (DM.fromList [(x, (0, x)) | x <- [0..n-1]])
  foundCliques <- newChan
  return (penaltySTM, foundCliques)


valid graph clique = and [connected graph (nodes!!x) (nodes!!y) |
                          x <- [0..(m-1)], y <- [(x+1)..(m-1)]]
  where nodes = setToList n clique
        n = nodeCount graph
        m = length nodes
        inClique x = clique `testBit` x

main :: IO ()
main = do
  [filename, spd, steps, wanted] <- getArgs
  file <- B.readFile filename
  case parseByteString "" file of
    Right (GraphEdges n _ e) ->
      do
        shared@(mvPM, chan) <- getSharedState n
        let graph = createGraph n e
        forM [1..8] $ \x -> do
          forkIO $ do
            putStrLn "Buscando"
            goDLS graph Settings { graph = graph,
                                   maxSteps = read steps,
                                   penaltyDelay = read spd,
                                   sharedPenalties = mvPM,
                                   cliqueChan = chan}
            putStrLn "Listo."
        cliques <- getChanContents chan
        forM cliques $ \clique -> do
          putStrLn ("Se consiguio un clique de tamano " ++(show (popCount clique)))
          putStrLn $ show (valid graph clique)
          if popCount clique >= (read wanted)
            then exitSuccess
            else return ()
        putStrLn "Muerete que chao."
    Left e -> error "Nope."
