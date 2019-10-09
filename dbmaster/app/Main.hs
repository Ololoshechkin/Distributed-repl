{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns     #-}

module Main where

import Network.Transport (Connection, send)
import Control.Concurrent (putMVar, readMVar, modifyMVar_, isEmptyMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Par.Class (fork)
import Data.Map ((!))
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
import System.Random.Shuffle (shuffleM)

-- 1 sec
workersTimeoutMicros :: Integer
workersTimeoutMicros = 250000

totalTimeoutMicros :: Int
totalTimeoutMicros = 500000

main :: IO ()
main = runDefaultMaster $ \server curCid bytes -> do
  let query = ((B.decode $ BS.fromStrict bytes) :: DBMessage)
  liftIO $ putStrLn $ "query : " ++ (show query)
  case query of
    RegisterDBWorker -> do
      let cs = view connMap server
      conn <- liftIO $ readMVar (cs ! curCid)
      noWorkersYet <- liftIO $ isEmptyMVar $ view workersList server
      if noWorkersYet
        then liftIO $ putMVar (view workersList server) [conn]
        else liftIO $ modifyMVar_ (view workersList server) $ return . (conn : )
      return ()
    PublishMessage key value repl -> do
      let msg = PublishMessage key value 1
      workers <- liftIO $ chooseKWorkers server repl
      !_ <- liftIO $ putStrLn $ "workers : " ++ (show $ length workers)
      _ <- forM workers $ \worker -> void $ liftIO $ send worker [BS.toStrict $ B.encode msg]
      return ()
    LoadMessage key -> do
      _ <- fork $ void $ liftIO $ timeout totalTimeoutMicros $ do
        workers <- readMVar $ view workersList server
        let waitingMap = view isWaiting server
        liftIO $ atomically $ writeTVar (waitingMap ! curCid) True
        let workerMsg = LoadMessageWithID curCid key
        _ <- forM workers $ \worker -> void $ liftIO $ send worker [BS.toStrict $ B.encode workerMsg]
        _ <- liftIO $ delay workersTimeoutMicros
        stillWaiting <- liftIO $ fmap not $ readTVarIO $ (view isWaiting server) ! curCid
        if stillWaiting
          then do
            let err = LoadError $ "No data for the given key \"" ++ key ++ "\" available in the moment"
            conn <- liftIO $ readMVar ((view connMap server) ! curCid)
            _ <- liftIO $ send conn [BS.toStrict $ B.encode err]
            return ()
          else return ()
      return ()
    LoadResultForID cid val -> do
      let waiting = view isWaiting server
      let curWaiting = (waiting ! curCid)
      liftIO $ atomically $ writeTVar curWaiting False
      let res = LoadResult val
      conn <- liftIO $ readMVar ((view connMap server) ! cid)
      _ <- liftIO $ send conn [BS.toStrict $ B.encode res]
      return ()
    LoadErrorForID _ _ -> return ()
    _ -> liftIO $ putStrLn $ "Unexpected result: " ++ (show query)

-- Note: chooeses N (or less) random workers to send data to
chooseKWorkers :: ServerMaster -> Int -> IO [Connection]
chooseKWorkers server wokrersCnt = do
  workers  <- readMVar $ view workersList server
  fmap (take wokrersCnt) $ shuffleM workers

