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
    ) where

import           Control.Monad.State        (MonadState, MonadIO, runStateT, put, get, StateT, liftIO)
import qualified Data.Map.Strict            as Map
import           Messages
import           Control.Monad.Catch

data Value = IntValue Int | StringValue String | BoolValue Bool | LambdaValue LambdaDef deriving (Show)

type VariableMap = Map.Map String Value

data CompilationException = CompilationException String deriving (Show)

instance Exception CompilationException

newtype ScriptContext t = ScriptContext {runStmt :: StateT VariableMap IO t}
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState VariableMap
    , MonadThrow
    , MonadCatch
    )

type PublishCallback = (String -> String -> Int -> IO ())
type LoadCallback = (String -> IO DBMessage)

data DB = DB { publish :: PublishCallback
             , load    :: LoadCallback}

executeScript :: Program -> DB -> IO ProgramResult
executeScript prog db = execScript Map.empty prog
  where
    execScript :: VariableMap -> Program -> IO ProgramResult
    execScript initMap = (executeScriptWithContext initMap) . executeProgramWithErrors

    executeScriptWithContext :: VariableMap -> ScriptContext t -> IO t
    executeScriptWithContext initMap ctx = fmap fst (runStateT (runStmt ctx) initMap)

    handleCE :: (Monad t) => CompilationException -> t ProgramResult
    handleCE (CompilationException descr) = return $ CompilationError descr

    executeProgramWithErrors :: (MonadState VariableMap t, MonadIO t, MonadCatch t) => Program -> t ProgramResult
    executeProgramWithErrors program = catch (fmap (Success . show) (executeProgram program)) handleCE

    executeProgram :: (MonadState VariableMap t, MonadIO t, MonadThrow t) => Program -> t Value
    executeProgram (Program statements retStatement) = do
      _ <- executeStatements statements
      evaluateReturnStatement retStatement

    evaluateReturnStatement :: (MonadState VariableMap t, MonadIO t, MonadThrow t) => ReturnStatement -> t Value
    evaluateReturnStatement (ReturnStatement expr) = evaluateExpr expr

    executeStatements :: (MonadState VariableMap t, MonadIO t, MonadThrow t) => [Statement] -> t ()
    executeStatements [] = return ()
    executeStatements (s : r) = do
      _ <- executeStatement s
      executeStatements r

    executeStatement :: (MonadState VariableMap t, MonadIO t, MonadThrow t) => Statement -> t ()
    executeStatement s  = case s of
      AssignmentStatement (Assignment var expr) -> do
        value <- evaluateExpr expr
        map_  <- get
        put $ Map.insert var value map_
      LoopStatement loop -> executeLoop loop
      InvocationStatement inv -> do
        _ <- evaluateExpr $ InvocationExpression inv
        return ()
      DBComandStatement cmd -> evaluateDBComand cmd

    evaluateDBComand :: (MonadState VariableMap t, MonadIO t, MonadThrow t) => DBComand -> t ()
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
              map_  <- get
              put $ Map.insert name (StringValue vs) map_
            LoadError e   -> throwM $ CompilationException $ "LOAD of the key " ++ (show k) ++ " returned error:\n" ++ e
            _             -> throwM $ CompilationException $ "Unexpected result for LOAD " ++ (show k) ++ ":\n" ++ (show dbResp)
        _ -> throwM $ CompilationException $ "DB comands operate on strings only, found : " ++ (show k) ++ " as a key for LOAD"

    executeLoop :: (MonadState VariableMap t, MonadIO t, MonadThrow t) => Loop -> t ()
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


    evaluateExprs :: (MonadState VariableMap t, MonadIO t, MonadThrow t) => [Expression] -> t [Value]
    evaluateExprs [] = return []
    evaluateExprs (e : r) = do
      s <- evaluateExpr e
      rs <- evaluateExprs r
      return $ s : rs

    evaluateExpr :: (MonadState VariableMap t, MonadIO t, MonadThrow t) => Expression -> t Value
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
      map_  <- get
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
      map_  <- get
      case (Map.lookup name map_) of
        Just value -> return value
        Nothing -> throwM $ CompilationException $ "Variable \"" ++ name ++ "\" was not found"

    invokeLambda :: LambdaDef -> [Value] -> IO Value
    invokeLambda (LambdaDef names program) values = do
      let map_ = Map.fromList $ zip names values
      let progExec = (executeScriptWithContext map_) . executeProgram
      progExec program

