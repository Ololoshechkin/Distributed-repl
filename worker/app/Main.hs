{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Par.Class (fork)
import Control.Monad.Par.IO (ParIO, runParIO)
import Network.Transport (Connection, EndPoint, Event(..), send, receive)
import Network.Socket.Internal (withSocketsDo)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy       as BS (fromStrict, toStrict)
import qualified Data.Binary                as B (decode, encode)
import Messages (Message (..), DBMessage (..))
import NetworkUtils (connectToServer)
import Lib

main :: IO ()
main = withSocketsDo $ do
  [serverAddr, dbAddr, port1, port2] <- getArgs
  tryConnectToServer serverAddr port1 $ \masterConn masterEndpoint -> do
    tryConnectToServer dbAddr port2 $ \dbConn dbEndpoint -> do
      workerServer masterConn masterEndpoint dbConn dbEndpoint

tryConnectToServer :: String -> String -> (Connection -> EndPoint -> IO ()) -> IO ()
tryConnectToServer serverAddr port action = do
  connOr <- connectToServer serverAddr port
  case connOr of
    Left errMsg -> putStrLn errMsg
    Right (masterConn, masterEndpoint) -> action masterConn masterEndpoint

data ServerWorker = ServerWorker

workerServer :: Connection -> EndPoint -> Connection -> EndPoint -> IO ()
workerServer masterConn masterEndpoint dbConn dbEndpoint = do
  _ <- send masterConn [BS.toStrict $ B.encode RegisterWorker]
  _ <- receive dbEndpoint -- ConnectionOpened
  runParIO $ go ServerWorker
  where
    go :: ServerWorker -> ParIO ()
    go server = do
      event <- liftIO $ receive masterEndpoint
      liftIO $ putStrLn $ "event : " ++ (show event)
      case event of
        ConnectionOpened _ _ _ -> go server
        Received _ (bytes : _) -> do
          _ <- fork $ do
            let query = ((B.decode $ BS.fromStrict bytes) :: Message)
            case query of
              CompileWorkerRequest cid program -> do
                result <- liftIO $ executeScript program $ DB { publish = publishImpl, load = loadImpl }
                liftIO $ void $ send masterConn [BS.toStrict $ B.encode (CompileWorkerReply cid result)]
                return ()
              _ -> do return ()
          go server
        _ -> return ()
    publishImpl :: String -> String -> Int -> IO ()
    publishImpl key value repl = do
      let req = PublishMessage key value repl
      _ <- liftIO $ send dbConn [BS.toStrict $ B.encode req]
      return ()
    loadImpl :: String -> IO DBMessage
    loadImpl key = do
      let req = LoadMessage key
      _ <- liftIO $ send dbConn [BS.toStrict $ B.encode req]
      event <- liftIO $ receive dbEndpoint
      case event of
        Received _ (bytes : _) -> return ((B.decode $ BS.fromStrict bytes) :: DBMessage)
        _                      -> return $ LoadError $ show event