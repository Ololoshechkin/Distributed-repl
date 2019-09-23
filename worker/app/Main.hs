{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (void)
import Network.Transport
import Network.Socket.Internal (withSocketsDo)
import System.Environment
import Control.Concurrent
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
  go ServerWorker
  where
    go :: ServerWorker -> IO ()
    go server = do
      event <- receive masterEndpoint
      putStrLn $ "event : " ++ (show event)
      case event of
        ConnectionOpened _ _ _ -> go server
        Received _ (bytes : _) -> do
          _ <- forkIO $ do
            let query = ((B.decode $ BS.fromStrict bytes) :: Message)
            case query of
              CompileWorkerRequest cid program -> do
                result <- executeScript program $ DB { publish = publishImpl, load = loadImpl }
                void $ send masterConn [BS.toStrict $ B.encode (CompileWorkerReply cid result)]
                return ()
              _ -> do return ()
          go server
        _ -> return ()
    publishImpl :: String -> String -> Int -> IO ()
    publishImpl key value repl = do
      let req = PublishMessage key value repl
      _ <- send dbConn [BS.toStrict $ B.encode req]
      return ()
    loadImpl :: String -> IO DBMessage
    loadImpl key = do
      let req = LoadMessage key
      _ <- send dbConn [BS.toStrict $ B.encode req]
      event <- receive dbEndpoint
      case event of
        Received _ (bytes : _) -> return ((B.decode $ BS.fromStrict bytes) :: DBMessage)
        _                      -> return $ LoadError $ show event