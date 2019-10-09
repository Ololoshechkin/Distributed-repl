{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module NetworkUtils where

import Data.ByteString.Char8 (pack)
import Data.Either (Either)
import System.Environment (getArgs)
import Network.Transport (ConnectionId, Connection, EndPoint, Reliability(..), EndPointAddress(..), Event(..), close, defaultConnectHints, newEndPoint, connect, receive, address)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Network.Socket (withSocketsDo)
import qualified Data.ByteString            as B (ByteString)
import qualified Data.ByteString.Lazy       as BS (fromStrict, toStrict)
import Control.Concurrent (MVar, putMVar, readMVar, newEmptyMVar)
import Control.Lens hiding (element)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Par (IVar(..))
import Control.Monad.Par.Class (fork, get, put, new)
import Control.Monad.Par.IO (ParIO(..), runParIO)
import qualified Data.Map                   as Map
import Data.Map ((!))
import Control.Concurrent.STM.TVar (TVar, newTVarIO)

data ServerMaster = ServerMaster { _connMap     :: Map.Map ConnectionId (MVar Connection)
                                 , _workersList :: MVar [Connection]
                                 , _isWaiting   :: Map.Map ConnectionId (TVar Bool)
                                 }

$(makeLenses ''ServerMaster) 

connectToServer :: String -> String -> IO (Either String (Connection, EndPoint))
connectToServer serverAddr port = do
  let host = "127.0.0.1"
  Right transport <- createTransport host port (const (host, port)) defaultTCPParameters
  Right endpoint  <- newEndPoint transport

  let addr = EndPointAddress (pack serverAddr)
  connRes <- connect endpoint addr ReliableOrdered defaultConnectHints
  case connRes of
    Left err   -> return $ Left $ show err
    Right conn -> return $ Right $ (conn, endpoint)

type MasterRequestHandler = ServerMaster -> ConnectionId -> B.ByteString -> ParIO ()

runDefaultMaster :: MasterRequestHandler -> IO ()
runDefaultMaster requestHandler = withSocketsDo $ runParIO $ do
  [host, port]    <- liftIO $ getArgs
  serverDone      <- new
  Right transport <- liftIO $ createTransport host port (const (host, port)) defaultTCPParameters
  Right endpoint  <- liftIO $ newEndPoint transport
  _ <- fork $ runMasterImpl endpoint serverDone requestHandler
  liftIO $ putStrLn $ "Master started at " ++ show (address endpoint)
  get serverDone
  where
    runMasterImpl :: EndPoint -> IVar () -> MasterRequestHandler -> ParIO ()
    runMasterImpl endpoint serverDone requestHandler = do
      workers <- liftIO $ newEmptyMVar
      go $ ServerMaster Map.empty workers Map.empty
      where
        go :: ServerMaster -> ParIO ()
        go server = do
          event <- liftIO $ receive endpoint
          case event of
            ConnectionOpened cid rel addr -> do
              liftIO $ putStrLn "ConnectionOpened"
              connMVar <- liftIO $ newEmptyMVar
              _ <- fork $ do
                Right conn <- liftIO $ connect endpoint addr rel defaultConnectHints
                liftIO $ putMVar connMVar conn 
              waitingTVar <- liftIO $ newTVarIO False
              go $ over isWaiting (Map.insert cid waitingTVar) $ over connMap (Map.insert cid connMVar) server
            Received curCid (bytes : []) -> do
              liftIO $ putStrLn $ "Received : " ++ (show bytes)
              _ <- fork $ requestHandler server curCid bytes
              go server
            ConnectionClosed cid -> do
              liftIO $ putStrLn "ConnectionClosed"
              _ <- fork $ do
                let cs = view connMap server
                conn <- liftIO $ readMVar (cs ! cid)
                liftIO $ close conn 
              go $ over connMap (Map.delete cid) server
            EndPointClosed -> do
              liftIO $ putStrLn "EndPointClosed"
              liftIO $ putStrLn "Master Server server exiting"
              put serverDone ()
            _ -> do
              liftIO $ putStrLn $ "unknown event : " ++ (show event)
              go server