{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib
import System.Environment
import Network.Socket.Internal (withSocketsDo)

main :: IO ()
main = withSocketsDo $ do
  [serverAddr, port, workerType] <- getArgs
  tryConnectToServer serverAddr port $ \masterConn masterEndpoint -> do
    case workerType of 
      "-sql" -> do
        dbworker <- makeSQLDBWorker
        workerServer dbworker masterConn masterEndpoint
      _      -> do
        dbworker <- makeFSDBWorker
        workerServer dbworker masterConn masterEndpoint
      