{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Network.Transport
import Network.Socket.Internal (withSocketsDo)
import System.Environment
import qualified Data.ByteString.Lazy       as BS (fromStrict, toStrict)
import qualified Data.Binary                as B (decode, encode)
import Messages (DBMessage (..))
import NetworkUtils (connectToServer)
import System.IO (IOMode (..), openFile, hClose, hPutStr, hIsEOF, hGetLine)

main :: IO ()
main = withSocketsDo $ do
  [serverAddr, port] <- getArgs
  tryConnectToServer serverAddr port $ \masterConn masterEndpoint -> do
      workerServer masterConn masterEndpoint

tryConnectToServer :: String -> String -> (Connection -> EndPoint -> IO ()) -> IO ()
tryConnectToServer serverAddr port action = do
  connOr <- connectToServer serverAddr port
  case connOr of
    Left errMsg -> putStrLn errMsg
    Right (masterConn, masterEndpoint) -> action masterConn masterEndpoint

data DBWorker = DBWorker

workerServer :: Connection -> EndPoint -> IO ()
workerServer masterConn masterEndpoint = do
  _ <- send masterConn [BS.toStrict $ B.encode RegisterDBWorker]
  go DBWorker
  where
    go :: DBWorker -> IO ()
    go server = do
      event <- receive masterEndpoint
      case event of
        ConnectionOpened _ _ _ -> go server
        Received _ (bytes : _) -> do
          let query = ((B.decode $ BS.fromStrict bytes) :: DBMessage)
          case query of
            PublishMessage key value _ -> do
              h <- openFile key WriteMode
              hPutStr h value
              hClose h
              go server
            LoadMessageWithID cid key -> do
              h <- openFile key ReadMode
              eof <- hIsEOF h
              if eof
                then do
                  let err = LoadErrorForID cid $ "No key \"" ++ (show key) ++ "\" found"
                  _ <- send masterConn [BS.toStrict $ B.encode err]
                  hClose h
                  go server
                else do
                  val <- hGetLine h
                  let res = LoadResultForID cid val
                  _ <- send masterConn [BS.toStrict $ B.encode res]
                  hClose h
                  go server
            _ -> go server
        _ -> go server