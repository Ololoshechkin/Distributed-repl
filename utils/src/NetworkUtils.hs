{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module NetworkUtils where

import Data.ByteString.Char8 (pack)
import Data.Either
import System.Environment
import Network.Transport
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Network.Socket (withSocketsDo)
import qualified Data.ByteString            as B (ByteString)
import qualified Data.ByteString.Lazy       as BS (fromStrict, toStrict)
import Control.Lens hiding (element)
import Control.Concurrent
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

type MasterRequestHandler = ServerMaster -> ConnectionId -> B.ByteString -> IO ()

runDefaultMaster :: MasterRequestHandler -> IO ()
runDefaultMaster requestHandler = withSocketsDo $ do
  [host, port]    <- getArgs
  serverDone      <- newEmptyMVar
  Right transport <- createTransport host port (const (host, port)) defaultTCPParameters
  Right endpoint  <- newEndPoint transport
  _ <- forkIO $ runMasterImpl endpoint serverDone requestHandler
  putStrLn $ "Master started at " ++ show (address endpoint)
  readMVar serverDone >>= print where
    runMasterImpl :: EndPoint -> MVar () -> MasterRequestHandler -> IO ()
    runMasterImpl endpoint serverDone requestHandler = do
      workers <- newEmptyMVar
      go $ ServerMaster Map.empty workers Map.empty
      where
        go :: ServerMaster -> IO ()
        go server = do
          event <- receive endpoint
          case event of
            ConnectionOpened cid rel addr -> do
              putStrLn "ConnectionOpened"
              connMVar <- newEmptyMVar
              _ <- forkIO $ do
                Right conn <- connect endpoint addr rel defaultConnectHints
                putMVar connMVar conn 
              waitingTVar <- newTVarIO False
              go $ over isWaiting (Map.insert cid waitingTVar) $ over connMap (Map.insert cid connMVar) server
            Received curCid (bytes : []) -> do
              putStrLn $ "Received : " ++ (show bytes)
              _ <- forkIO $ requestHandler server curCid bytes
              go server
            ConnectionClosed cid -> do
              putStrLn "ConnectionClosed"
              _ <- forkIO $ do
                let cs = view connMap server
                conn <- readMVar (cs ! cid)
                close conn 
              go $ over connMap (Map.delete cid) server
            EndPointClosed -> do
              putStrLn "EndPointClosed"
              putStrLn "Master Server server exiting"
              putMVar serverDone ()
            _ -> do
              putStrLn $ "unknown event : " ++ (show event)
              go server