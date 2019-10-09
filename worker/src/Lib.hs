{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE BangPatterns               #-}
module Lib
    ( 
          executeScript
        , DB (..)
        , PublishCallback
        , LoadCallback
        , UploadMapCallback
        , DownloadMapCallback
    ) where

import Network.Transport (ConnectionId)
import           Control.Monad.Reader (MonadReader, MonadIO, runReaderT, ask, ReaderT, liftIO)
import qualified Data.Map.Strict            as Map
import           Data.IORef                 (IORef, modifyIORef, newIORef, readIORef)
import           Messages
import           Control.Monad.Catch
import           Text.Read                  (readMaybe)


data CompilationException = CompilationException String deriving (Show)

instance Exception CompilationException

newtype ScriptContext t = ScriptContext {runStmt :: ReaderT (IORef VariableMap) IO t}
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (IORef VariableMap)
    , MonadThrow
    , MonadCatch
    )

type PublishCallback = (String -> String -> Int -> IO ())
type LoadCallback = (String -> IO DBMessage)
type DownloadMapCallback = (ConnectionId -> IO DBMessage)
type UploadMapCallback = (ConnectionId -> VariableMap -> IO ())

data DB = DB { publish     :: PublishCallback
             , load        :: LoadCallback
             , downloadMap :: DownloadMapCallback
             , uploadMap   :: UploadMapCallback}

type PredefinedFunction = [Value] -> Either CompilationException Value

executeScript :: Program -> ConnectionId -> DB -> IO ProgramResult
executeScript prog cid db = execScript prog
  where
    toString :: PredefinedFunction
    toString [] = Right $ StringValue ""
    toString (x : []) = Right $ StringValue $ showValue x
    toString (x : r) = case toString r of
      Right (StringValue suffix) -> Right $ StringValue $ (showValue x) ++ ", " ++ suffix
      ex                         -> ex
    
    showValue :: Value -> String
    showValue v = case v of 
      IntValue x -> (show x)
      StringValue x -> (show x)
      BoolValue x -> (show x)
      LambdaValue (LambdaDef names _) -> "lambda" ++ (show names)

    toInt :: PredefinedFunction
    toInt [] = Left $ CompilationException "Function toInt expectes exactly 1 argument but 0 were provided"
    toInt (v : []) = case v of 
      IntValue _ -> Right $ v
      StringValue s -> case (readMaybe s :: Maybe Int) of 
        Just x -> Right $ IntValue x
        _      -> Left $ CompilationException $ "String \"" ++ s  ++ "\" can't be converted to Int type"
      BoolValue b   -> case b of
       True  -> Right $ IntValue 1
       False -> Right $ IntValue 0
      LambdaValue _ -> Left $ CompilationException $ "Lambdas can't be converted to Int type"
    toInt args = Left $ CompilationException $ "Function toInt expectes exactly 1 argument but " ++ (show $ length args) ++  " were provided"

    toBool :: PredefinedFunction
    toBool [] = Left $ CompilationException "Function toBool expectes exactly 1 argument but 0 were provided"
    toBool (v : []) = case v of
     IntValue x    -> Right $ BoolValue $ x == 0
     StringValue s -> Right $ BoolValue $ s == "True"
     BoolValue _   -> Right $ v
     LambdaValue _ -> Left $ CompilationException $ "Lambdas can't be converted to Int type"
    toBool args = Left $ CompilationException $ "Function toInt expectes exactly 1 argument but " ++ (show $ length args) ++  " were provided"

    substring :: PredefinedFunction
    substring args = case args of
      (sv : lv : lenv : []) -> case sv of 
        StringValue s -> case lv of
          IntValue l -> case lenv of
            IntValue len -> Right $ StringValue $ drop l $ take (l + len) s
            _          -> Left  $ CompilationException $ "Function substring expectes int as third argument but " ++ (show lenv) ++  " were provided"
          _ -> Left $ CompilationException $ "Function substring expectes int as second argument but " ++ (show lv) ++  " were provided"
        _ -> Left $ CompilationException $ "Function substring expectes string as first argument but " ++ (show sv) ++  " were provided"
      _ -> Left $ CompilationException $ "Function substring expectes exactly 3 arguments but " ++ (show $ length args) ++  " were provided"

    execScript :: Program -> IO ProgramResult
    execScript (ReplProgramSegment st) = do
      dbResp <- liftIO $ (downloadMap db) cid
      case dbResp of 
        ResultMap _ initMap -> (executeScriptWithContext initMap) $ executeProgramWithErrors (ReplProgramSegment st)
        LoadError e -> return $ CompilationError $ "Failed to access database: " ++ e
        _           -> return $ CompilationError $ "Unexpected response from DB: " ++ (show dbResp)
    execScript p = (executeScriptWithContext Map.empty) $ executeProgramWithErrors p

    executeScriptWithContext :: VariableMap -> ScriptContext t -> IO t
    executeScriptWithContext initMap ctx = do
      mapRef <- newIORef initMap
      runReaderT (runStmt ctx) mapRef

    handleCE :: (Monad t) => CompilationException -> t ProgramResult
    handleCE (CompilationException descr) = return $ CompilationError descr

    executeProgramWithErrors :: (MonadReader (IORef VariableMap) t, MonadIO t, MonadCatch t) => Program -> t ProgramResult
    executeProgramWithErrors program = catch (fmap (Success . show) (executeProgram program)) handleCE

    executeProgram :: (MonadReader (IORef VariableMap) t, MonadIO t, MonadThrow t) => Program -> t Value
    executeProgram (Program statements retStatement) = do
      _ <- executeStatements statements
      evaluateReturnStatement retStatement
    executeProgram (ReplProgramSegment st) = do
      res     <- executeReplStatement st
      mapRef  <- ask
      map_    <- liftIO $ readIORef mapRef
      _       <- liftIO $ (uploadMap db) cid map_
      return res

    evaluateReturnStatement :: (MonadReader (IORef VariableMap) t, MonadIO t, MonadThrow t) => ReturnStatement -> t Value
    evaluateReturnStatement (ReturnStatement expr) = evaluateExpr expr

    executeReplStatement :: (MonadReader (IORef VariableMap) t, MonadIO t, MonadThrow t) => ReplStatement -> t Value
    executeReplStatement (NormalStatement st) = do
      _ <- executeStatement st
      case st of 
        AssignmentStatement (Assignment name _) -> do
          mapRef  <- ask
          map_    <- liftIO $ readIORef mapRef
          case (Map.lookup name map_) of
            Just value -> return value
            Nothing    -> throwM $ CompilationException $ "Variable \"" ++ name ++ "\" occasionally failed to be assigned"
        _ -> return $ StringValue ""
    executeReplStatement (EvalExprStatement expr) = evaluateExpr expr

    executeStatements :: (MonadReader (IORef VariableMap) t, MonadIO t, MonadThrow t) => [Statement] -> t ()
    executeStatements [] = return ()
    executeStatements (s : r) = do
      _ <- executeStatement s
      executeStatements r

    executeStatement :: (MonadReader (IORef VariableMap) t, MonadIO t, MonadThrow t) => Statement -> t ()
    executeStatement s  = case s of
      AssignmentStatement (Assignment var expr) -> do
        value <- evaluateExpr expr
        map_  <- ask
        liftIO $ modifyIORef map_ (Map.insert var value)
      LoopStatement loop -> executeLoop loop
      InvocationStatement inv -> do
        _ <- evaluateExpr $ InvocationExpression inv
        return ()
      DBComandStatement cmd -> evaluateDBComand cmd

    evaluateDBComand :: (MonadReader (IORef VariableMap) t, MonadIO t, MonadThrow t) => DBComand -> t ()
    evaluateDBComand (PublishDBComand (PublishComand ke ve r)) = do
      k <- evaluateExpr ke
      case k of
        StringValue ks -> do
          v <- evaluateExpr ve
          case v of
            StringValue vs -> liftIO $ (publish db) ks vs r
            _ -> throwM $ CompilationException $ "DB comands operate on strings only, found : " ++ (show v) ++ " as a value for PUBLISH"
        _ -> throwM $ CompilationException $ "DB comands operate on strings only, found : " ++ (show k) ++ " as a key for PUBLISH"
    evaluateDBComand (LoadDBComand (LoadComand ke name)) = do
      k <- evaluateExpr ke
      case k of
        StringValue ks -> do
          dbResp <- liftIO $ (load db) ks
          case dbResp of 
            LoadResult vs -> do
              map_ <- ask
              liftIO $ modifyIORef map_ (Map.insert name (StringValue vs))
            LoadError e   -> throwM $ CompilationException $ "LOAD of the key " ++ (show k) ++ " returned error:\n" ++ e
            _             -> throwM $ CompilationException $ "Unexpected result for LOAD " ++ (show k) ++ ":\n" ++ (show dbResp)
        _ -> throwM $ CompilationException $ "DB comands operate on strings only, found : " ++ (show k) ++ " as a key for LOAD"

    executeLoop :: (MonadReader (IORef VariableMap) t, MonadIO t, MonadThrow t) => Loop -> t ()
    executeLoop (While cond statements) = do
      value <- evaluateExpr cond
      case value of 
        BoolValue b -> if b
          then do
            _ <- executeStatements statements
            executeLoop (While cond statements)
          else
            return ()
        _          -> throwM $ CompilationException $ "Loop condition \"" ++ (show cond) ++ "\" has incorrect type (bool expected)"


    evaluateExprs :: (MonadReader (IORef VariableMap) t, MonadIO t, MonadThrow t) => [Expression] -> t [Value]
    evaluateExprs [] = return []
    evaluateExprs (e : r) = do
      s <- evaluateExpr e
      rs <- evaluateExprs r
      return $ s : rs

    evaluateExpr :: (MonadReader (IORef VariableMap) t, MonadIO t, MonadThrow t) => Expression -> t Value
    evaluateExpr (OperatorExpression operator) = do
      case operator of 
        UnaryOperator opType op expr -> do
          value <- evaluateExpr expr
          case value of 
            IntValue i -> do
              if opType == IntType
                then do
                  case op of 
                    "+" -> return $ IntValue i
                    "-" -> return $ IntValue $ -i
                    _   -> throwM $ CompilationException $ "Unknown unary operator \"" ++ op ++ "\" for integer type"
                else do
                  throwM $ CompilationException $ "Unexpected operator \"" ++ op ++ "\" applied to integer expression"
            BoolValue b -> do
              if opType == IntType
                then do
                  case op of 
                    "!" -> return $ BoolValue $ not b
                    _   -> throwM $ CompilationException $ "Unknown unary operator \"" ++ op ++ "\" for bool type"
                else do
                  throwM $ CompilationException $ "Unexpected operator \"" ++ op ++ "\" applied to integer expression"
            StringValue _ -> throwM $ CompilationException "String expression is not compatible with unary operators"
            LambdaValue _ -> throwM $ CompilationException "Unexpected lambda expression in operator statement"
        BinaryOperator opType op leftExpr rightExpr -> do
          leftValue <- evaluateExpr leftExpr
          rightValue <- evaluateExpr rightExpr
          case leftValue of 
            IntValue l -> do
              case rightValue of 
                IntValue r -> do
                  if opType == IntType
                    then do
                      case op of 
                        "+" -> return $ IntValue $ l + r
                        "-" -> return $ IntValue $ l - r
                        "*" -> return $ IntValue $ l * r
                        "/" -> return $ IntValue $ l `div` r
                        "==" -> return $ BoolValue $ l == r
                        "/=" -> return $ BoolValue $ l /= r
                        "<" -> return $ BoolValue $ l < r
                        _   -> throwM $ CompilationException $ "Unknown Binary operator \"" ++ op ++ "\" for integer types"
                    else do
                      throwM $ CompilationException $ "Unexpected operator \"" ++ op ++ "\" applied to integer expressions"
                _ -> do
                  throwM $ CompilationException $ "Incompatible types : integer (type of " ++ (show leftValue) ++ ") and type of (" ++ (show rightValue) ++ ")"
            BoolValue l -> do
              case rightValue of 
                BoolValue r -> do
                  if opType == BoolType
                    then do
                      case op of 
                        "||" -> return $ BoolValue $ l || r
                        "&&" -> return $ BoolValue $ l && r
                        _   -> throwM $ CompilationException $ "Unknown Binary operator \"" ++ op ++ "\" for bool types"
                    else do
                      throwM $ CompilationException $ "Unexpected operator \"" ++ op ++ "\" applied to bool expressions"
                _ -> do
                  throwM $ CompilationException $ "Incompatible types : bool (type of " ++ (show leftValue) ++ ") and type of (" ++ (show rightValue) ++ ")"
            StringValue l -> do
              case rightValue of 
                StringValue r -> do
                  if opType == StringType
                    then do
                      case op of 
                        "##" -> return $ StringValue $ l ++ r
                        "::" -> return $ BoolValue $ l == r
                        _   -> throwM $ CompilationException $ "Unknown Binary operator \"" ++ op ++ "\" for string types"
                    else do
                      throwM $ CompilationException $ "Unexpected operator \"" ++ op ++ "\" applied to string expressions"
                _ -> do
                  throwM $ CompilationException $ "Incompatible types : string (type of " ++ (show leftValue) ++ ") and type of (" ++ (show rightValue) ++ ")"
            LambdaValue _ -> throwM $ CompilationException "Unexpected lambda expression in operator statement"

    evaluateExpr (InvocationExpression (Invocation name args)) = do
      case name of 
        "toString" -> do
          values <- evaluateExprs args
          case toString values of
            Left  e -> throwM e
            Right v -> return v
        "toInt"    ->  do
          values <- evaluateExprs args
          case toInt values of
            Left  e -> throwM e
            Right v -> return v
        "toBool"    ->  do
          values <- evaluateExprs args
          case toBool values of
            Left  e -> throwM e
            Right v -> return v
        "substring"    ->  do
          values <- evaluateExprs args
          case substring values of
            Left  e -> throwM e
            Right v -> return v
        _          -> do
          mapRef  <- ask
          map_    <- liftIO $ readIORef mapRef
          case (Map.lookup name map_) of
            Just value -> case value of 
              LambdaValue lambda -> do
                values <- evaluateExprs args
                res <- liftIO $ invokeLambda lambda values
                return res
              _ -> throwM $ CompilationException $ "Variable \"" ++ name ++ "\" is not a lambda"
            Nothing -> throwM $ CompilationException $ "Lambda variable \"" ++ name ++ "\" was not found"
    evaluateExpr (ConstantExpression c) = do
      case c of 
        IntConstant i    -> return $ IntValue i
        StringConstant s -> return $ StringValue s
        BoolConstant b   -> return $ BoolValue b
    evaluateExpr (IfThenElseExpression (IfThenElse cond alt1 alt2)) = do
      value <- evaluateExpr cond
      case value of
        BoolValue ok -> do
          if ok
            then evaluateExpr alt1
            else evaluateExpr alt2
        _ -> throwM $ CompilationException $ "Expected bool value in if statemend, found : " ++ (show value) 
    evaluateExpr (LambdaDefExpression lambda) = return $ LambdaValue lambda
    evaluateExpr (BracesExpression e) = evaluateExpr e
    evaluateExpr (VariableExpression name) = do
      mapRef  <- ask
      map_    <- liftIO $ readIORef mapRef
      case (Map.lookup name map_) of
        Just value -> return value
        Nothing -> throwM $ CompilationException $ "Variable \"" ++ name ++ "\" was not found"

    invokeLambda :: LambdaDef -> [Value] -> IO Value
    invokeLambda (LambdaDef names program) values = do
      let map_ = Map.fromList $ zip names values
      let progExec = (executeScriptWithContext map_) . executeProgram
      progExec program

