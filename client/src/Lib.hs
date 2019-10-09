{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Lib where

import           Data.Void                  (Void)
import           Text.Megaparsec            -- TODO: (<|>) is not working with manual imports
import           Text.Megaparsec.Char       (space, space1, char, string, letterChar, alphaNumChar)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme, space)
import           Messages

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

skipWord :: String -> Parser ()
skipWord w = do
  _ <- space
  lexeme (string w *> notFollowedBy letterChar)

identifier :: Parser String
identifier = lexeme $ some letterChar

integer :: Parser Int
integer = lexeme L.decimal

-- * Assignment = 'var' Identifier '=' Expression
-- * Loop = 'while' BracesExpression '{' (Statement*) '}' 
-- * Expression = Invocation | Constant | OperatorExpression | IfThenElseExpression | LambdaDefExpression | BracesExpression
-- * Invocation = Identifier '(' Expression (',' Expression)* ')'
-- * Identifier = [a-z]+ (note : names like "function", "return", "if" or other keywords are not allowed. If you use such names nothing can be guaranteed)
-- * Constant = IntConstant | StringConstant | BoolConstant
-- * IntConstant = [0-9]+ (note: 00091 <=> 91)
-- * StringConstant = \"[a-z0-9A-Z_]*\"
-- * BoolConstant = 'true' | 'false' 
-- * OperatorExpression : UnaryOpExpr | BinaryOpExpr
-- * UnaryOpExpr = UnaryOp BracesExpression
-- * BinaryOpExpr = BracesExpression BinaryOp BracesExpression
-- * UnaryOp : '+' | '-' | '!'
-- * BinaryOp : '+' | '-' | '*' | '/' | '==' | '!=' | '&&' | '||'
-- * IfThenElseExpression = 'if' BracesExpression 'then' BracesExpression 'else' BracesExpression
-- * LambdaDefExpression = 'function' Identifier '(' (Identifier (',' Identifier)*)? ')' '{' Program '}'
-- * BracesExpression = '(' Expression ')'
-- * DBComand = PublishComand | LoadComand
-- * PublishComand = 'PUBLISH' Key Value ReplicationFactor 
-- * Key = BracesExpression of type String
-- * Value = BracesExpression of type String
-- * ReplicationFactor = IntConstant
-- * LoadComand = 'LOAD' Key Identifier 
-- * Key = BracesExpression of type String
-- * ReturnStatement = 'return' Expression


parseProgram :: Parser Program
parseProgram = do
  statement       <- (many parseStatement)
  returnStatement <- parseReturnStatement
  return $ Program statement returnStatement

parseStatement :: Parser Statement
parseStatement = try $ do
    statement <- ((fmap LoopStatement parseLoop) <|> (fmap AssignmentStatement parseAssignment) <|> (fmap DBComandStatement parseDBComand) <|> (fmap InvocationStatement parseInvocation))
    return statement

parseAssignment :: Parser Assignment
parseAssignment = try $ do
    _    <- skipWord "var"
    name <- identifier
    _    <- lexeme $ char '='
    expr <- parseExpression
    return $ Assignment name expr

parseExpression :: Parser Expression
parseExpression = (fmap ConstantExpression parseConstant)
              <|> (fmap OperatorExpression parseOperator)
              <|> (fmap IfThenElseExpression parseIfThenElse)
              <|> (fmap LambdaDefExpression parseLambdaDef)
              <|> (fmap InvocationExpression parseInvocation)
              <|> parseBracesExpression
              <|> parseVariableExpression

parseLoop :: Parser Loop
parseLoop = try $ do
    _          <- skipWord "while"
    cond       <- parseBracesExpression
    _          <- skipWord "{"
    statements <- many parseStatement
    _          <- skipWord "}"
    return $ While cond statements

parseDBComand :: Parser DBComand
parseDBComand = try ((fmap PublishDBComand parsePublishComand) <|> (fmap LoadDBComand parseLoadComand))

parsePublishComand :: Parser PublishComand
parsePublishComand = try $ do
    _                 <- skipWord "PUBLISH"
    key               <- parseExpression
    value             <- parseExpression
    replicationFactor <- integer
    return $ PublishComand key value replicationFactor

parseLoadComand :: Parser LoadComand
parseLoadComand = try $  do
    _    <- skipWord "LOAD"
    key  <- parseBracesExpression
    name <- identifier
    return $ LoadComand key name


parseInvocation :: Parser Invocation
parseInvocation = try $ do
    funName <- identifier
    _       <- char '('
    args    <- many parseExpression
    _       <- char ')'
    return $ Invocation funName args

parseConstant :: Parser Constant
parseConstant = (parseIntConstant <|> parseStringConstant <|> parseBoolConstant) where
    parseIntConstant = try $ (fmap IntConstant integer)
    parseStringConstant = try $ do
        _      <- char '\"'
        str <- many (letterChar <|> alphaNumChar)
        _      <- char '\"'
        return $ StringConstant str
    parseBoolConstant = fmap BoolConstant ((fmap (\_ -> True) (skipWord "true")) <|> (fmap (\_ -> False) (skipWord "false")))

parseOperator :: Parser Operator
parseOperator = parseUnary <|> parseBinary

parseUnary :: Parser Operator
parseUnary = parsePlus <|> parseMinus <|> parseNot where
    parsePlus = try $ do
        _ <- char '+'
        expr <- parseBracesExpression
        return $ UnaryOperator IntType "+" expr
    parseMinus = try $ do
        _ <- char '-'
        expr <- parseBracesExpression
        return $ UnaryOperator IntType "-" expr
    parseNot = try $ do
        _ <- char '!'
        expr <- parseBracesExpression
        return $ UnaryOperator BoolType "!" expr

parseBinary :: Parser Operator
parseBinary =  (parseBinaryImpl "+" (BinaryOperator IntType))
           <|> (parseBinaryImpl "-" (BinaryOperator IntType))
           <|> (parseBinaryImpl "*" (BinaryOperator IntType)) 
           <|> (parseBinaryImpl "/" (BinaryOperator IntType))
           <|> (parseBinaryImpl "||" (BinaryOperator BoolType))
           <|> (parseBinaryImpl "&&" (BinaryOperator BoolType))
           <|> (parseBinaryImpl "##" (BinaryOperator StringType))
           <|> (parseBinaryImpl "::" (BinaryOperator StringType))
           <|> (parseBinaryImpl "==" (BinaryOperator IntType))
           <|> (parseBinaryImpl "/=" (BinaryOperator IntType))
           <|> (parseBinaryImpl "<" (BinaryOperator IntType)) where
    parseBinaryImpl symbol makeOperator = try $ do
        a  <- parseBracesExpression
        _ <- skipWord symbol
        b  <- parseBracesExpression
        return $ makeOperator symbol a b

parseIfThenElse :: Parser IfThenElse
parseIfThenElse = try $ do
    _    <- skipWord "if"
    cond <- parseBracesExpression
    _    <- skipWord "then"
    alt1 <- parseBracesExpression
    _    <- skipWord "else"
    alt2 <- parseBracesExpression
    return $ IfThenElse cond alt1 alt2

parseLambdaDef :: Parser LambdaDef
parseLambdaDef = try $ do
    _    <- skipWord "function"
    _    <- char '('
    args <- many identifier
    _    <- skipWord ") {"
    prog <- parseProgram
    _    <- skipWord "}"
    return $ LambdaDef args prog

parseBracesExpression :: Parser Expression
parseBracesExpression = do
    _    <- lexeme $ char '('
    expr <- parseExpression
    _    <- lexeme $ char ')'
    return $ BracesExpression expr

parseVariableExpression :: Parser Expression
parseVariableExpression = fmap VariableExpression identifier

parseReturnStatement :: Parser ReturnStatement
parseReturnStatement = try $ do
  _    <- skipWord "return"
  expr <- parseExpression
  return $ ReturnStatement expr

parseReplStatementImpl :: Parser ReplStatement
parseReplStatementImpl = (try $ fmap EvalExprStatement parseOnlyExpression) <|> (try $ fmap NormalStatement parseStatement) where
  parseOnlyExpression = do
    res <- parseExpression
    _   <- eof
    return res

parseScript :: String -> Either String Program
parseScript input = case parse parseProgram "" input of
  Left ex -> Left (show ex)
  Right prog -> Right prog

parseReplStatement :: String -> Either String Program
parseReplStatement input = case parse parseReplStatementImpl "" input of
  Left ex -> Left (show ex)
  Right stat -> Right $ ReplProgramSegment stat

