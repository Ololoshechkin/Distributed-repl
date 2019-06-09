{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE BangPatterns          #-}
module Lib
    ( runScript
    ) where

import           Control.Monad.State        (MonadIO, MonadState, StateT, get,
                                             liftIO, put, runStateT)
import           Data.List                  (intercalate, filter)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Void                  (Void)
import           System.Directory           (getCurrentDirectory, setCurrentDirectory)
import           Text.Megaparsec
import           Text.Megaparsec.Char
-- import System.IO.Unsafe
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

type VariableMap = Map.Map String String

data Variable = Variable {name :: String} deriving (Show)

data Expression = VariableExpression Variable
    | TextExpression Text
    | RichTextExpression RichText
    | ComandExpression Comand deriving (Show)

data Comand = Read [Variable]
    | Echo Bool [Expression]
    | Pwd
    | Cd Expression
    | Exit deriving (Show)

data Text = Text {text :: String} deriving (Show)

data RichText = EmptyText
    | PlainText String RichText
    | VariableLink Variable RichText
    | ComandLink Comand RichText
    | EscapeChar Char RichText deriving (Show)

data ScriptUnit = Assignment {var :: String, expr :: Expression}
    | TopLevelComand Comand
    | EmptyLine String deriving (Show)

data Script = Script [ScriptUnit] deriving (Show)

sp :: Parser ()
sp = skipSome (char ' ')

spaceConsumer :: Parser ()
spaceConsumer = L.space sp empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

skipWord :: String -> Parser ()
skipWord w = lexeme (string w *> notFollowedBy alphaNumChar)

identifier :: Parser String
identifier = do
  c0  <- letterChar
  c1n <- many (letterChar <|> alphaNumChar)
  return (c0 : c1n)

integer :: Parser Int
integer = lexeme L.decimal

contains :: (Eq a) => [a] -> a -> Bool
contains [] _ = False
contains (x:rest) e = (e == x) || (contains rest e)

notContains :: (Eq a) => [a] -> a -> Bool
notContains arr e = not (contains arr e)

notCharsGreedy :: [Char] -> Parser String
notCharsGreedy chars = many (satisfy (notContains chars))

parseVarName :: Parser String
parseVarName = identifier <|> (fmap show integer)

parseVar :: Parser Variable
parseVar = parseWithDollar Variable parseVarName

parseWithBrackets :: Char -> Char -> (a -> b) -> Parser a -> Parser b
parseWithBrackets lBracket rBracket aToB aParser = do
  _ <- (char lBracket)
  a <- aParser
  _ <- (char rBracket)
  return $ aToB a

parseWithDollar :: (a -> b) -> Parser a -> Parser b
parseWithDollar aTob prsr = do
  _ <- char '$'
  a_ <- prsr
  return $ aTob a_

parseText :: Parser Text
parseText = parseWithBrackets '\'' '\'' Text (notCharsGreedy ['\''])

parseRichText :: Parser RichText
parseRichText = parseRichTextInBrackets <|> parseRichTextWithoutBrackets

parseRichTextImpl :: [Char] -> Parser RichText
parseRichTextImpl badChars = do
  -- let !_ = unsafePerformIO $ putStrLn ("parseRichTextImpl " ++ (show badChars))
  parseEscapeChar <|> (try parseVariableLink) <|> parseComandLink
    <|> parsePlainText
   where
    parseEscapeChar = do
      _ <- char '\\'
      c <- ((char '\\') <|> (char '$') <|> (char '\"'))
      -- let !_ = unsafePerformIO $ putStrLn ("parseEscapeChar " ++ (show c))
      fmap (EscapeChar c) (parseRichTextImpl badChars)
    parseVariableLink = do
      v <- parseVar
      -- let !_ = unsafePerformIO $ putStrLn ("parseVariableLink " ++ (show v))
      fmap (VariableLink v) (parseRichTextImpl badChars)
    parseComandLink = do
      cmd <- parseComandInBrackets
      -- let !_ = unsafePerformIO $ putStrLn ("parseComandLink " ++ (show cmd))
      fmap (ComandLink cmd) (parseRichTextImpl badChars)
    parsePlainText = do
      txt <- notCharsGreedy badChars
      case txt of
        [] -> do
          -- let !_ = unsafePerformIO $ putStrLn ("EMPTY_TEXT")
          return EmptyText
        _ -> do
          -- let !_ = unsafePerformIO $ putStrLn ("parsePlainText: " ++ txt)
          fmap (PlainText txt) (parseRichTextImpl badChars)

parseRichTextInBrackets :: Parser RichText
parseRichTextInBrackets = parseWithBrackets '\"' '\"' id (parseRichTextImpl ['\\', '$', '\"'])

parseRichTextWithoutBrackets :: Parser RichText
parseRichTextWithoutBrackets = try $ do
  parsedText <- parseRichTextImpl ['\\', '$', '\"', ' ', '\n']
  case parsedText of
    EmptyText -> empty
    _ -> return parsedText

parseExpr :: Parser Expression
parseExpr = do
  -- let !_ = unsafePerformIO $ putStrLn "---parseExpr----"
  lexeme ((try parseComandExpr) <|> (try parseVarExpr) <|> (try parseTextExpr) <|> (try parseRichTextExpr)) where
  parseVarExpr      = do
    res <- fmap VariableExpression parseVar
    -- let !_ = unsafePerformIO $ putStrLn ("parseVarExpr" ++ (show res))
    return res
  parseTextExpr     = do
    res <- fmap TextExpression parseText
    -- let !_ = unsafePerformIO $ putStrLn ("parseTextExpr" ++ (show res))
    return res
  parseRichTextExpr = do
    res <- fmap RichTextExpression parseRichText
    -- let !_ = unsafePerformIO $ putStrLn ("parseRichTextExpr" ++ (show res))
    return res
  parseComandExpr   = do
    res <- fmap ComandExpression parseComandInBrackets
    -- let !_ = unsafePerformIO $ putStrLn ("parseComandExpr" ++ (show res))
    return res

parseComandInBrackets :: Parser Comand
parseComandInBrackets = parseWithDollar id (parseWithBrackets '(' ')' id parseComand)

parseLine :: Parser ScriptUnit
parseLine = (try parseAssignment) <|> (try parseTopLevelComand) <|> (fmap EmptyLine parseEOL)

parseEOL :: Parser String
parseEOL = eol <|> (fmap (\_ -> "") eof) <|> (fmap (:[]) (char ';')) <|> (fmap (:[]) (char ' '))

parseAssignment :: Parser ScriptUnit
parseAssignment = do
  -- let !_ = unsafePerformIO $ putStrLn ("parseAssignment")
  var_  <- lexeme parseVarName
  _     <- (char '=')
  expr_ <- parseExpr
  -- let !_ = unsafePerformIO $ putStrLn ("parseAssignment !!")
  _     <- parseEOL
  return $ Assignment var_ expr_

parseTopLevelComand :: Parser ScriptUnit
parseTopLevelComand = (fmap TopLevelComand parseComand) <* parseEOL

parseComandImpl :: (Show a) => String -> (a -> Comand) -> Parser a -> Parser Comand
parseComandImpl keyword constructor argsParser = do
  _    <- skipWord keyword
  -- let !_ = unsafePerformIO $ putStrLn keyword
  args <- argsParser
  -- let !_ = unsafePerformIO $ putStrLn ("args: " ++ show args)
  return $ constructor args

parseComandWithFlagImpl :: (Show a) => String -> (Bool -> a -> Comand) -> Parser Bool -> Parser a -> Parser Comand
parseComandWithFlagImpl keyword constructor flagParser argsParser = do
  _    <- skipWord keyword
  flag <- flagParser
  -- let !_ = unsafePerformIO $ putStrLn keyword
  args <- argsParser
  -- let !_ = unsafePerformIO $ putStrLn ("args: " ++ show args)
  return $ constructor flag args

parseComand :: Parser Comand
parseComand = parseReadCmd <|> (try parseEchoCmd) <|> parsePwdCmd
  <|> parseCdCmd <|> parseExitCmd where
    parseReadCmd = parseComandImpl "read" Read (many $ fmap Variable (lexeme parseVarName))
    parsePwdCmd  = parseComandImpl "pwd"  id   (return Pwd)
    parseCdCmd   = parseComandImpl "cd"   Cd   ((try parseExpr) <|> (fmap (TextExpression . Text) (notCharsGreedy ['\n'])))
    parseExitCmd = parseComandImpl "exit" id   (return Exit)
    parseEchoCmd = parseComandWithFlagImpl "echo" Echo flagParser (many $ lexeme parseExpr) where
      flagParser = (fmap (\_ -> True) (lexeme $ string "-n")) <|> (return False)

scriptParser :: Parser Script
scriptParser = fmap Script (many parseLine)

newtype ScriptContext t = ScriptContext {runStmt :: StateT VariableMap IO t}
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState VariableMap
    )

executeScriptWithContext :: VariableMap -> ScriptContext t -> IO ()
executeScriptWithContext initMap ctx = do
  _ <- runStateT (runStmt ctx) initMap
  return ()

executeImpl :: (MonadState VariableMap t, MonadIO t) => [ScriptUnit] -> t ()
executeImpl units = case units of
  [] -> return ()
  (unit:rest) -> do
    case unit of
      TopLevelComand cmd  -> case cmd of
        Exit      -> return ()
        _ -> do
          _ <- evaluateComand True cmd
          executeImpl rest
      Assignment v e      -> do
        value <- evaluateExpr e
        map_  <- get
        put $ Map.insert v value map_
        executeImpl rest
      EmptyLine _         -> executeImpl rest

evaluateExpr :: (MonadState VariableMap t, MonadIO t) => Expression -> t String
evaluateExpr e = case e of
  VariableExpression var_ -> evaluateVar var_
  TextExpression (Text txt)      -> return txt
  RichTextExpression rTxt -> evaluateRichText rTxt
  ComandExpression cmd    -> evaluateComand False cmd

evaluateRichText :: (MonadState VariableMap t, MonadIO t) => RichText -> t String
evaluateRichText rTxt = do
  case rTxt of
    EmptyText              -> return ""
    PlainText txt rich     -> do
      suffix <- evaluateRichText rich
      return $ txt ++ suffix
    VariableLink var_ rich -> do
      value  <- evaluateVar var_
      suffix <- evaluateRichText rich
      return $ value ++ suffix
    ComandLink cmd rich    -> do
      cmdRes  <- evaluateComand False cmd
      suffix <- evaluateRichText rich
      return $ cmdRes ++ suffix
    EscapeChar c rich      -> do
      suffix <- evaluateRichText rich
      return (c : suffix)

evaluateVar :: (MonadState VariableMap t) => Variable -> t String
evaluateVar (Variable var_) = do
  map_ <- get
  return $ fromMaybe "" (Map.lookup var_ map_)

evaluateComand :: (MonadState VariableMap t, MonadIO t) => Bool -> Comand -> t String
evaluateComand isTopLevel cmd = case cmd of
  Read variables -> if isTopLevel
    then do
      line <- liftIO getLine
      _ <- assignVars (map name variables) line ""
      return ""
    else return ""
  Echo flag exprs -> do
    values        <- mapM evaluateExpr exprs
    let fltr = if flag then (/= '\n') else (\_ -> True)
    let stringToPrint = filter fltr ((intercalate " " values) ++ "\n")
    if isTopLevel
    then do
      liftIO $ putStr stringToPrint
      return ""
    else return stringToPrint
  Pwd -> do
    pwd <- liftIO getCurrentDirectory
    if isTopLevel
      then do
        liftIO $ putStrLn pwd
        return ""
      else return pwd
  Cd expr_ -> if isTopLevel
    then do
      path <- evaluateExpr expr_
      _    <- liftIO $ setCurrentDirectory path
      return ""
    else return ""
  Exit -> return ""

assignVars :: (MonadState VariableMap t, MonadIO t) => [String] -> String -> String -> t ()
assignVars [] _ _ = return ()
assignVars (x:r) [] c = do
  map_ <- get
  put $ Map.insert x c map_
  assignVars r [] ""
assignVars (x:[]) l _ = do
  map_ <- get
  put $ Map.insert x l map_
  return ()
assignVars (x:r) (c:rc) cur = if c == ' '
  then do
    map_ <- get
    put $ Map.insert x cur map_
    assignVars r rc ""
  else assignVars (x:r) rc (cur ++ [c])


execScript :: VariableMap -> [ScriptUnit] -> IO ()
execScript initMap = (executeScriptWithContext initMap) . executeImpl

runScript_ :: String -> VariableMap -> IO ()
runScript_ input args = do
  case parse scriptParser "" input of
    (Right (Script units)) -> execScript args units
    Left _ -> error ("bad input : \n" ++ input)

runScript :: FilePath -> VariableMap -> IO ()
runScript file args = do
    input <- readFile $ file
    runScript_ input args
