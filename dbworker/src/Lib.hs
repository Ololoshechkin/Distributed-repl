{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns     #-}

module Lib where

import Network.Transport                         (Connection, EndPoint, Event(..), ConnectionId, send, receive)
import qualified Data.ByteString.Lazy         as BS (fromStrict, toStrict, pack, unpack)
import qualified Data.Binary                  as B (decode, encode)
import qualified Data.ByteString              as DBS (ByteString, writeFile, readFile)
import qualified Data.Map                     as Map (empty)
import qualified Database.SQLite.Simple       as SQL (Connection(..), Only(..), field, execute, execute_, open, queryWith, withTransaction)
import Messages (DBMessage (..), VariableMap)
import NetworkUtils (connectToServer)
import System.IO (IOMode (..), openFile, hClose, hPutStr, hIsEOF, hGetLine)
import System.EasyFile (doesFileExist)


tryConnectToServer :: String -> String -> (Connection -> EndPoint -> IO ()) -> IO ()
tryConnectToServer serverAddr port action = do
  connOr <- connectToServer serverAddr port
  case connOr of
    Left errMsg -> putStrLn errMsg
    Right (masterConn, masterEndpoint) -> action masterConn masterEndpoint


type PublishHandler = String -> String -> IO ()
type LoadHandler = String -> ConnectionId -> IO DBMessage
type UploadMapHandler = ConnectionId -> VariableMap -> IO ()
type DownloadMapHandler = ConnectionId -> ConnectionId -> IO DBMessage

class DBWorker a where
  publish     :: a -> PublishHandler
  load        :: a -> LoadHandler
  uploadMap   :: a -> UploadMapHandler
  downloadMap :: a -> DownloadMapHandler

data FileSystemDBWorker = FileSystemDBWorker
instance DBWorker FileSystemDBWorker where
  publish _ key value = do
    h <- openFile key WriteMode
    hPutStr h value
    hClose h
  load _ key cid = do
    exists <- doesFileExist key
    let err = LoadErrorForID cid $ "No key \"" ++ (show key) ++ "\" found"
    if exists
      then do
        h <- openFile key ReadMode
        eof <- hIsEOF h
        if eof
          then do
            hClose h
            return err
          else do
            val <- hGetLine h
            let res = LoadResultForID cid val
            hClose h
            return res
      else return err
  uploadMap _ clientCid m = do
    let fname = ((show clientCid) ++ ".map")
    DBS.writeFile fname $ BS.toStrict $ B.encode m
  downloadMap _ workerCid clientCid = do
    let fname = ((show clientCid) ++ ".map")
    exists <- doesFileExist fname
    if exists
      then do
        content <- DBS.readFile fname
        let map_  = ((B.decode $ BS.fromStrict $ content) :: VariableMap)
        return $ ResultMapForId workerCid clientCid map_
      else return $ ResultMapForId workerCid clientCid Map.empty

data SQLiteDBWorker = SQLiteDBWorker { connection :: SQL.Connection }
instance DBWorker SQLiteDBWorker where
  publish worker key value = do
    let conn = connection worker
    let insert = "INSERT or REPLACE INTO kvmaptable (key, value) VALUES (?,?)"
    _ <- SQL.withTransaction conn $ do
      SQL.execute conn insert (key, value)
      return ()
    return ()
  load worker key cid = do
    let conn         = connection worker
    let selectQuery  = "SELECT value from kvmaptable where key = ?"
    let getAllForKey = (SQL.queryWith SQL.field conn selectQuery (SQL.Only key)) :: IO [String]
    let err          = LoadErrorForID cid $ "No key \"" ++ (show key) ++ "\" found"
    let makeReply    = fmap (\arr -> if (null arr) then err else LoadResultForID cid (head arr)) getAllForKey
    res <- SQL.withTransaction conn makeReply
    return res
  uploadMap worker cid m = do
    let conn = connection worker
    let insert = "INSERT or REPLACE INTO repltable (user, varmap) VALUES (?,?)"
    _ <- SQL.withTransaction conn $ do
      SQL.execute conn insert (cid, BS.toStrict $ B.encode m)
      return ()
    return ()
  downloadMap worker workerCid clientCid = do
    let conn         = connection worker
    let selectQuery  = "SELECT varmap from repltable where user = ?"
    let getAllForKey = (SQL.queryWith SQL.field conn selectQuery (SQL.Only clientCid)) :: IO [DBS.ByteString]
    let makeReply    = fmap (\arr -> if (null arr) then ResultMapForId workerCid clientCid Map.empty else ResultMapForId workerCid clientCid ((B.decode $ BS.fromStrict $ head arr) :: VariableMap)) getAllForKey
    res <- SQL.withTransaction conn makeReply
    return res

makeFSDBWorker :: IO FileSystemDBWorker
makeFSDBWorker = return FileSystemDBWorker

makeSQLDBWorker :: IO SQLiteDBWorker
makeSQLDBWorker = do
  conn <- SQL.open "kvmaptable.db"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS kvmaptable (key TEXT PRIMARY KEY, value TEXT)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS repltable (user TEXT PRIMARY KEY, varmap TEXT)"
  return $ SQLiteDBWorker conn

workerServer :: (DBWorker a) => a -> Connection -> EndPoint -> IO ()
workerServer dbserver masterConn masterEndpoint = do
  _ <- send masterConn [BS.toStrict $ B.encode RegisterDBWorker]
  go dbserver
  where
    go :: (DBWorker a) => a -> IO ()
    go server = do
      event <- receive masterEndpoint
      case event of
        ConnectionOpened _ _ _ -> go server
        Received _ (bytes : _) -> do
          let query = ((B.decode $ BS.fromStrict bytes) :: DBMessage)
          case query of
            PublishMessage key value _ -> do
              !_ <- putStrLn "(publish server) k v"
              (publish server) key value
              go server
            LoadMessageWithID cid key -> do
              !_ <- putStrLn "(load server) k c"
              reply <- (load server) key cid
              _ <- send masterConn [BS.toStrict $ B.encode reply]
              go server
            UploadMap cid m -> do
              !_ <- putStrLn "(uploadMap server) cid m"
              (uploadMap server) cid m
              go server
            DownloadMapForId workerCid clientCid -> do
              !_ <- putStrLn "(downloadMap server) c1 c2"
              reply <- (downloadMap server) workerCid clientCid
              _ <- send masterConn [BS.toStrict $ B.encode reply]
              go server
            _ -> go server
        _ -> go server