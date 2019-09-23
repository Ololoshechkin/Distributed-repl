{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Network.Transport
import Control.Concurrent
import Data.Map ((!))
import System.Random
import qualified Data.ByteString.Lazy       as BS (fromStrict, toStrict)
import qualified Data.Binary                as B (decode, encode)
import Messages (Message (..), Program)
import Control.Lens hiding (element)
import Data.Bifunctor
import NetworkUtils

-- _connMap     :: Map.Map ConnectionId (MVar Connection)
--                                      , _workersList :: MVar [Connection]
--                                      , _extraMap    :: Map.Map k (MVar v)

main :: IO ()
main = runDefaultMaster $ \server curCid bytes -> do
  let cs = view connMap server
  let query = ((B.decode $ BS.fromStrict bytes) :: Message)
  putStrLn $ "query : " ++ (show query)
  case query of
    RegisterWorker -> do
      putStrLn $ "RegisterWorker"
      conn <- readMVar (cs ! curCid)
      noWorkersYet <- isEmptyMVar $ view workersList server
      if noWorkersYet
        then putMVar (view workersList server) [conn]
        else modifyMVar_ (view workersList server) $ return . (conn : )
      return ()
    CompileClientRequest program -> chooseWorkerLoop server program curCid 0
    CompileWorkerReply cid result -> do
      putStrLn $ "CompileWorkerReply"
      conn <- readMVar (cs ! cid)
      let clientReply = CompileClientReply result
      _ <- send conn [BS.toStrict $ B.encode clientReply]
      return ()
    invalidReq -> do
      putStrLn $ "invalid request : " ++ (show invalidReq)
      return ()

maxAttempts :: Int
maxAttempts = 100

-- chose random worker until one of the following is true
-- * worker successfully receives the message
-- * maxAttempts expired
chooseWorkerLoop :: ServerMaster -> Program -> ConnectionId -> Int -> IO ()
chooseWorkerLoop server program curCid attempts = do
  if attempts == maxAttempts
    then return ()
    else do
      workers <- readMVar $ view workersList server
      let n = length workers
      i <- fmap (`mod` n) (randomIO :: IO Int)
      let workerConn = workers !! i
      let workerRequest = CompileWorkerRequest curCid program
      res <- send workerConn [BS.toStrict $ B.encode workerRequest]
      case res of
        Left _ -> do
          modifyMVar_ (view workersList server) $ return . (deleteIth i)
          chooseWorkerLoop server program curCid (attempts + 1)
        _ -> return ()

deleteIth :: Int -> [a] -> [a]
deleteIth n = uncurry (++) . second (drop 1) . splitAt n