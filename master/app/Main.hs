{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Network.Transport (send)
import Control.Concurrent (putMVar, readMVar, modifyMVar_, isEmptyMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Par.IO (ParIO)
import Data.Map ((!))
import System.Random (randomIO)
import qualified Data.ByteString.Lazy       as BS (fromStrict, toStrict)
import qualified Data.Binary                as B (decode, encode)
import Messages (Message (..))
import Control.Lens hiding (element)
import Data.Bifunctor (second)
import NetworkUtils

-- _connMap     :: Map.Map ConnectionId (MVar Connection)
--                                      , _workersList :: MVar [Connection]
--                                      , _extraMap    :: Map.Map k (MVar v)

main :: IO ()
main = runDefaultMaster $ \server curCid bytes -> do
  let cs = view connMap server
  let query = ((B.decode $ BS.fromStrict bytes) :: Message)
  liftIO $ putStrLn $ "query : " ++ (show query)
  case query of
    RegisterWorker -> do
      liftIO $ putStrLn $ "RegisterWorker"
      conn <- liftIO $ readMVar (cs ! curCid)
      noWorkersYet <- liftIO $ isEmptyMVar $ view workersList server
      if noWorkersYet
        then liftIO $ putMVar (view workersList server) [conn]
        else liftIO $ modifyMVar_ (view workersList server) $ return . (conn : )
      return ()
    CompileClientRequest program -> chooseWorkerLoop server (CompileWorkerRequest curCid program) 0
    CompileWorkerReply cid result -> do
      liftIO $ putStrLn $ "CompileWorkerReply"
      conn <- liftIO $ readMVar (cs ! cid)
      let clientReply = CompileClientReply result
      _ <- liftIO $ send conn [BS.toStrict $ B.encode clientReply]
      return ()
    invalidReq -> do
      liftIO $ putStrLn $ "invalid request : " ++ (show invalidReq)
      return ()

maxAttempts :: Int
maxAttempts = 100

-- chose random worker until one of the following is true
-- * worker successfully receives the message
-- * maxAttempts expired
chooseWorkerLoop :: ServerMaster -> Message -> Int -> ParIO ()
chooseWorkerLoop server msg attempts = do
  if attempts == maxAttempts
    then return ()
    else do
      workers <- liftIO $ readMVar $ view workersList server
      let n = length workers
      i <- liftIO $ fmap (`mod` n) (randomIO :: IO Int)
      let workerConn = workers !! i
      res <- liftIO $ send workerConn [BS.toStrict $ B.encode msg]
      case res of
        Left _ -> do
          _ <- liftIO $ modifyMVar_ (view workersList server) $ return . (deleteIth i)
          chooseWorkerLoop server msg (attempts + 1)
        _ -> return ()

deleteIth :: Int -> [a] -> [a]
deleteIth n = uncurry (++) . second (drop 1) . splitAt n