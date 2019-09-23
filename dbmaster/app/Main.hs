{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Network.Transport
import Control.Concurrent
import Data.Map ((!))
import System.Random
import qualified Data.ByteString.Lazy       as BS (fromStrict, toStrict)
import qualified Data.Binary                as B (decode, encode)
import Messages (DBMessage (..))
import Control.Lens hiding (element)
import NetworkUtils
import Control.Concurrent.Thread.Delay (delay)
import System.Timeout (timeout)
import Control.Monad (void, forM)
import Control.Concurrent.STM.TVar (writeTVar, readTVarIO)
import Control.Monad.STM (atomically)

-- 1 sec
workersTimeoutMicros :: Integer
workersTimeoutMicros = 250000

totalTimeoutMicros :: Int
totalTimeoutMicros = 500000

main :: IO ()
main = runDefaultMaster $ \server curCid bytes -> do
  let query = ((B.decode $ BS.fromStrict bytes) :: DBMessage)
  putStrLn $ "query : " ++ (show query)
  case query of
    RegisterDBWorker -> do
      let cs = view connMap server
      conn <- readMVar (cs ! curCid)
      noWorkersYet <- isEmptyMVar $ view workersList server
      if noWorkersYet
        then putMVar (view workersList server) [conn]
        else modifyMVar_ (view workersList server) $ return . (conn : )
      return ()
    PublishMessage key value repl -> do
      let msg = PublishMessage key value 1
      workers <- chooseNWorkers server repl
      _ <- forM workers $ \worker -> void $ send worker [BS.toStrict $ B.encode msg]
      return ()
    LoadMessage key -> do
      _ <- forkIO $ void $ timeout totalTimeoutMicros $ do
        workers <- readMVar $ view workersList server
        let waitingMap = view isWaiting server
        atomically $ writeTVar (waitingMap ! curCid) True
        let workerMsg = LoadMessageWithID curCid key
        _ <- forM workers $ \worker -> void $ send worker [BS.toStrict $ B.encode workerMsg]
        _ <- delay workersTimeoutMicros
        stillWaiting <- fmap not $ readTVarIO $ (view isWaiting server) ! curCid
        if stillWaiting
          then do
            let err = LoadError $ "No data for the given key \"" ++ key ++ "\" available in the moment"
            conn <- readMVar ((view connMap server) ! curCid)
            _ <- send conn [BS.toStrict $ B.encode err]
            return ()
          else return ()
      return ()
    LoadResultForID cid val -> do
      let waiting = view isWaiting server
      let curWaiting = (waiting ! curCid)
      atomically $ writeTVar curWaiting False
      let res = LoadResult val
      conn <- readMVar ((view connMap server) ! cid)
      _ <- send conn [BS.toStrict $ B.encode res]
      return ()
    LoadErrorForID _ _ -> return ()
    _ -> putStrLn $ "Unexpected result: " ++ (show query)

-- Note: chooeses N (or less) random workers to send data to
chooseNWorkers :: ServerMaster -> Int -> IO [Connection]
chooseNWorkers server n = readMVar $ view workersList server