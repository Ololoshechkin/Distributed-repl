{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Network.Transport
import qualified Data.ByteString.Lazy       as BS (toStrict, fromStrict)
import qualified Data.Binary                as B (encode, decode)
import Data.List (intercalate)
import Messages ( Message( CompileClientRequest, CompileClientReply), ProgramResult (..) )
import NetworkUtils (connectToServer)
import Foreign.Hoppy.Runtime (withScopedPtr)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Widgets.QTextEdit as QTextEdit
import Graphics.UI.Qtah.Event (onEvent)
import qualified Graphics.UI.Qtah.Core.Types as Types
import qualified Graphics.UI.Qtah.Gui.QCloseEvent as QCloseEvent
import qualified Graphics.UI.Qtah.Gui.QFont as QFont
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QSplitter as QSplitter
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import qualified Graphics.UI.Qtah.Widgets.QFileDialog as QFileDialog
import Graphics.UI.Qtah.Signal (connect_)
import System.Environment (getArgs)
import System.Timeout (timeout)
import Control.Monad (unless)
import Lib

main :: IO ()
main = withScopedPtr (getArgs >>= QApplication.new) $ \_ -> do
  [serverAddr, port] <- getArgs
  connOr <- connectToServer serverAddr port
  case connOr of 
    Left errMsg -> showMessageWindow $ "Couldn't connect to server:\n" ++ errMsg
    Right (conn, endpoint) -> do
      _ <- receive endpoint
      uiWindow <- newMainWindow conn endpoint serverAddr
      QWidget.show uiWindow
  QCoreApplication.exec

label :: String -> IO QLabel.QLabel
label name = do
  titleLabel <- QLabel.newWithText (name :: String)
  titleFont <- QWidget.font titleLabel
  QFont.setPixelSize titleFont 25
  QWidget.setFont titleLabel titleFont
  return titleLabel

button :: String -> (Bool -> IO ()) -> IO QPushButton.QPushButton
button name action = do
    btn <- QPushButton.newWithText (name :: String)
    connect_ btn QAbstractButton.clickedSignal action
    return btn

textField :: IO QTextEdit.QTextEdit
textField = QTextEdit.new

splitterBox :: Types.QtOrientation -> Int -> Int -> (QSplitter.QSplitter -> IO ()) -> IO QSplitter.QSplitter
splitterBox orientation height width fill = do
  splitter <- QSplitter.newWithOrientation orientation
  QSplitter.setSizes splitter [height, width]
  fill splitter
  return splitter

horisontal :: Int -> Int -> (QSplitter.QSplitter -> IO ()) -> IO QSplitter.QSplitter
horisontal height width fill = splitterBox Types.Horizontal height width fill

vertical :: Int -> Int -> (QSplitter.QSplitter -> IO ()) -> IO QSplitter.QSplitter
vertical height width fill = splitterBox Types.Vertical height width fill

showMessageWindow :: String -> IO ()
showMessageWindow message = do
  window <- QWidget.new
  QWidget.setWindowTitle window ("Distributed Repl" :: String)
  QWidget.resizeRaw window 500 250
  caption <- label message
  layout <- QVBoxLayout.newWithParent window
  QBoxLayout.addWidget layout caption
  QWidget.show window

saveToFile :: QWidget.QWidget -> String -> IO ()
saveToFile window textToSave = do
  fileName <- QFileDialog.getSaveFileName window ("Save script" :: String) ("" :: String) ("All Files (*)" :: String)
  unless (null fileName) $ do
    writeFile fileName textToSave


loadFromFile :: QWidget.QWidget -> IO String
loadFromFile window = do
  fileName <- QFileDialog.getOpenFileName window ("Open script" :: String) ("" :: String) ("All Files (*)" :: String)
  if (null fileName)
    then do
      showMessageWindow "Sorry, file name can not be resolved, try again"
      return ""
    else readFile fileName

-- 1.5 sec
serverTimeoutMicros :: Int
serverTimeoutMicros = 1500000

newMainWindow :: Connection -> EndPoint -> String -> IO QWidget.QWidget
newMainWindow servConn servEndpoint serverAddr = do
  -- Create and initialize widgets.

  window <- QWidget.new
  QWidget.setWindowTitle window ("Distributed Repl" :: String)
  QWidget.resizeRaw window 1000 500

  codeField <- textField
  result <- label "(no result)"
  leftBox <- QWidget.new
  leftBoxLayout <- QVBoxLayout.new
  QWidget.setLayout leftBox leftBoxLayout
  QBoxLayout.addStretch leftBoxLayout
  QBoxLayout.addWidget leftBoxLayout codeField
  QBoxLayout.addWidget leftBoxLayout result

  compileButton <- button "&Compile" $ \_ -> do
    sourceCode <- QTextEdit.toPlainText codeField
    case parseScript sourceCode of
        Left errMessage  -> showMessageWindow $ "Failed to parse the script:\n" ++ errMessage
        Right program -> do
            let request = CompileClientRequest program
            sendRes <- send servConn ((BS.toStrict $ B.encode request) : [])
            case sendRes of 
                Right _ -> do
                    event <- timeout serverTimeoutMicros $ receive servEndpoint
                    case event of 
                        Just (Received _ (bytes : [])) -> do
                          let reply = (B.decode $ BS.fromStrict bytes) :: Message
                          case reply of
                            CompileClientReply programResult -> case programResult of
                              Success resString            -> QLabel.setText result resString
                              CompilationError description -> showMessageWindow ("Compilation error: \n" ++ description)
                            _                                         -> showMessageWindow "Unexpected reply from server"
                        Just (ConnectionClosed _) -> showMessageWindow "Server closed connection, sorry :("
                        Just _                    -> showMessageWindow "Try again! Something went wrong"
                        Nothing                   -> showMessageWindow "Server didn't respond in 1.5 sec, try again later\nor send script that can be executed faster"
                    putStrLn $ show event
                    return ()
                _ -> showMessageWindow "Failed to send message to server."
  replButton <- button "&Repl Mode" $ \_ -> showReplWindow serverAddr "0"
  quitButton <- button "&Quit" $ \_ -> QCoreApplication.quit
  saveButtin <- button "&Save" $ \_ -> do
    sourceCode <- QTextEdit.toPlainText codeField
    saveToFile window sourceCode
  loadButtin <- button "&Open" $ \_ -> do
    sourceCode <- loadFromFile window
    QTextEdit.setText codeField sourceCode

  rightBox <- QWidget.new
  rightBoxLayout <- QVBoxLayout.new
  QWidget.setLayout rightBox rightBoxLayout
  QBoxLayout.addStretch rightBoxLayout
  QBoxLayout.addWidget rightBoxLayout compileButton
  QBoxLayout.addWidget rightBoxLayout replButton
  QBoxLayout.addWidget rightBoxLayout saveButtin
  QBoxLayout.addWidget rightBoxLayout loadButtin
  QBoxLayout.addWidget rightBoxLayout quitButton

  horSpliter <- horisontal 200 300 $ \panel -> do
    QSplitter.addWidget panel leftBox
    QSplitter.addWidget panel rightBox
    return ()

  caption <- label "Source code"

  layout <- QVBoxLayout.newWithParent window
  QBoxLayout.addWidget layout caption
  QBoxLayout.addWidgetWithStretch layout horSpliter 1

  _ <- onEvent window $ \(_ :: QCloseEvent.QCloseEvent) -> do
    close servConn
    QCoreApplication.quit
    return False

  return window

showReplWindow :: String -> String -> IO ()
showReplWindow serverAddr port = do
  connOr <- connectToServer serverAddr port
  case connOr of 
    Left errMsg -> showMessageWindow $ "Couldn't connect to server:\n" ++ errMsg
    Right (conn, endpoint) -> do
      _ <- receive endpoint
      uiWindow <- newReplWindow conn endpoint
      QWidget.show uiWindow

newReplWindow :: Connection -> EndPoint -> IO QWidget.QWidget
newReplWindow servConn servEndpoint = do
  window <- QWidget.new
  QWidget.setWindowTitle window ("Distributed Repl" :: String)
  QWidget.resizeRaw window 1000 500

  code   <- label "Repl Mode:\n"
  codeField <- textField
  logs      <- label ""

  box <- QWidget.new
  boxLayout <- QVBoxLayout.new
  QWidget.setLayout box boxLayout
  QBoxLayout.addStretch boxLayout
  QBoxLayout.addWidget boxLayout code
  QBoxLayout.addWidget boxLayout codeField
  QBoxLayout.addWidget boxLayout logs

  replButon <- button "&Interpret" $ \_ -> do
    newCodeFragment <- QTextEdit.toPlainText codeField
    QTextEdit.clear codeField
    case parseReplStatement newCodeFragment of
        Left errMessage  -> showMessageWindow $ "Failed to parse the script:\n" ++ errMessage
        Right codeSegment -> do
            let request = CompileClientRequest codeSegment
            sendRes <- send servConn ((BS.toStrict $ B.encode request) : [])
            case sendRes of 
                Right _ -> do
                    event <- timeout serverTimeoutMicros $ receive servEndpoint
                    case event of 
                        Just (Received _ (bytes : [])) -> do
                          let reply = (B.decode $ BS.fromStrict bytes) :: Message
                          case reply of
                            CompileClientReply programResult -> do
                              oldCode <- QLabel.text code
                              let shrinkedCode = intercalate "\n" $ reverse $ take 12 $ reverse $ drop 1 $ lines oldCode
                              let newText = "Repl Mode:\n" ++ shrinkedCode ++ "\n" ++ newCodeFragment ++ "\n"
                              QLabel.setText code newText
                              case programResult of
                                Success resString            -> QLabel.setText logs $ take 50 resString
                                CompilationError description -> QLabel.setText logs $ take 50 $ "Error: " ++ description
                            _                                         -> showMessageWindow "Unexpected reply from server"
                        Just (ConnectionClosed _) -> showMessageWindow "Server closed connection, sorry :("
                        Just _                    -> showMessageWindow "Try again! Something went wrong"
                        Nothing                   -> showMessageWindow "Server didn't respond in 1.5 sec, try again later\nor send script that can be executed faster"
                    putStrLn $ show event
                    return ()
                _ -> showMessageWindow "Failed to send message to server."

  QBoxLayout.addWidget boxLayout replButon

  horSpliter <- horisontal 200 300 $ \panel -> do
    QSplitter.addWidget panel box
    return ()

  layout <- QVBoxLayout.newWithParent window
  QBoxLayout.addWidgetWithStretch layout horSpliter 1

  _ <- onEvent window $ \(_ :: QCloseEvent.QCloseEvent) -> do
    close servConn
    return False

  return window
