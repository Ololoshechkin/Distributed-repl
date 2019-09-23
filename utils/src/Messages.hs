module Messages where

import qualified Data.Binary                as B (Binary, Get, put, get)
import           Data.Word
import           Network.Transport (ConnectionId)

data DBMessage = PublishMessage String String Int 
               | LoadMessage String 
               | LoadMessageWithID ConnectionId String
               | LoadResult String 
               | LoadResultForID ConnectionId String
               | LoadError String
               | LoadErrorForID ConnectionId String
               | RegisterDBWorker deriving (Show)

instance B.Binary DBMessage where
    put (PublishMessage k v r) = do
        B.put (0 :: Word8)
        B.put k
        B.put v
        B.put r
    put (LoadMessage k) = do
        B.put (1 :: Word8)
        B.put k
    put (LoadMessageWithID i k) = do
        B.put (2 :: Word8)
        B.put i
        B.put k
    put (LoadResult v) = do
        B.put (3 :: Word8)
        B.put v
    put (LoadResultForID i v) = do
        B.put (4 :: Word8)
        B.put i
        B.put v
    put (LoadError e) = do
        B.put (5 :: Word8)
        B.put e
    put (LoadErrorForID i e) = do
        B.put (6 :: Word8)
        B.put i
        B.put e
    put RegisterDBWorker = B.put (7 :: Word8)
    get = do
        t <- B.get :: B.Get Word8
        case t of
            0 -> do
                k <- B.get
                v <- B.get
                r <- B.get
                return $ PublishMessage k v r
            1 -> do
                k <- B.get
                return $ LoadMessage k
            2 -> do
                i <- B.get
                k <- B.get
                return $ LoadMessageWithID i k
            3 -> do
                v <- B.get
                return $ LoadResult v
            4 -> do
                i <- B.get
                v <- B.get
                return $ LoadResultForID i v
            5 -> do
                e <- B.get
                return $ LoadError e
            6 -> do
                i <- B.get
                e <- B.get
                return $ LoadErrorForID i e
            _ -> return RegisterDBWorker

data Message = CompileClientRequest Program
             | CompileWorkerRequest ConnectionId Program
             | CompileWorkerReply ConnectionId ProgramResult
             | CompileClientReply ProgramResult
             | RegisterWorker deriving (Show)

instance B.Binary Message where
    put (CompileClientRequest program) = do
        B.put (0 :: Word8)
        B.put program
    put (CompileWorkerRequest cid program) = do
        B.put (1 :: Word8)
        B.put cid
        B.put program
    put (CompileWorkerReply cid result) = do
        B.put (2 :: Word8)
        B.put cid
        B.put result
    put (CompileClientReply result) = do
        B.put (3 :: Word8)
        B.put result
    put RegisterWorker = do
        B.put (4 :: Word8)
    get = do
        t <- B.get :: B.Get Word8
        case t of
            0 -> do
                program <- B.get
                return $ CompileClientRequest program
            1 -> do
                cid     <- B.get
                program <- B.get
                return $ CompileWorkerRequest cid program
            2 -> do
                cid   <- B.get
                result <- B.get
                return $ CompileWorkerReply cid result
            3 -> do
                result <- B.get
                return $ CompileClientReply result
            _ -> do
                return RegisterWorker

data ProgramResult = Success String | CompilationError String deriving (Show)
instance B.Binary ProgramResult where
    put (Success res) = do
        B.put (0 :: Word8)
        B.put res
    put (CompilationError descr) = do
        B.put (1 :: Word8)
        B.put descr
    get = do
        t <- B.get :: B.Get Word8
        case t of
            0 -> do
                res <- B.get
                return $ Success res
            _ -> do
                descr <- B.get
                return $ CompilationError descr

data Program = Program [Statement] ReturnStatement deriving (Show)

instance B.Binary Program where
    put (Program statements returnStatement) = do
        B.put statements
        B.put returnStatement
    get = do
        statements <- B.get
        returnStatement <- B.get
        return $ Program statements returnStatement

data Statement = AssignmentStatement Assignment
               | LoopStatement Loop
               | InvocationStatement Invocation
               | DBComandStatement DBComand deriving (Show)

instance B.Binary Statement where
    put (AssignmentStatement a) = do
        B.put (0 :: Word8)
        B.put a
    put (LoopStatement l) = do
        B.put (1 :: Word8)
        B.put l
    put (InvocationStatement i) = do
        B.put (2 :: Word8)
        B.put i
    put (DBComandStatement d) = do
        B.put (3 :: Word8)
        B.put d
    get = do
        t <- B.get :: B.Get Word8
        case t of 
            0 -> do
                a <- B.get
                return $ AssignmentStatement a
            1 -> do
                l <- B.get
                return $ LoopStatement l
            2 -> do
                i <- B.get
                return $ InvocationStatement i
            _ -> do
                d <- B.get
                return $ DBComandStatement d

data Assignment = Assignment String Expression deriving (Show)

instance B.Binary Assignment where
    put (Assignment s e) = do
        B.put s
        B.put e
    get = do
        s <- B.get
        e <- B.get
        return $ Assignment s e

data Loop = While Expression [Statement] deriving (Show)

instance B.Binary Loop where
    put (While e ss) = do
        B.put e
        B.put ss
    get = do
        e <- B.get
        ss <- B.get
        return $ While e ss

data Invocation = Invocation String [Expression] deriving (Show)

instance B.Binary Invocation where
    put (Invocation s ee) = do
        B.put s
        B.put ee
    get = do
        s <- B.get
        ee <- B.get
        return $ Invocation s ee 

data DBComand = PublishDBComand PublishComand | LoadDBComand LoadComand deriving (Show)

instance B.Binary DBComand where
    put (PublishDBComand pc) = do
        B.put (0 :: Word8)
        B.put pc
    put (LoadDBComand lc) = do
        B.put (1 :: Word8)
        B.put lc
    get = do
        t <- B.get :: B.Get Word8
        case t of 
            0 -> do
                pc <- B.get
                return $ PublishDBComand pc
            _ -> do
                lc <- B.get
                return $ LoadDBComand lc

data PublishComand = PublishComand Expression Expression Int deriving (Show)

instance B.Binary PublishComand where
    put (PublishComand k v r) = do
        B.put k
        B.put v
        B.put r
    get = do
        k <- B.get
        v <- B.get
        r <- B.get
        return $ PublishComand k v r

data LoadComand = LoadComand Expression String deriving (Show)

instance B.Binary LoadComand where
    put (LoadComand k n) = do
        B.put k
        B.put n
    get = do
        k <- B.get
        n <- B.get
        return $ LoadComand k n

data Expression = InvocationExpression Invocation
                | ConstantExpression Constant
                | OperatorExpression Operator
                | IfThenElseExpression IfThenElse
                | LambdaDefExpression LambdaDef
                | BracesExpression Expression 
                | VariableExpression String deriving (Show)

instance B.Binary Expression where
    put (InvocationExpression i) = do
        B.put (0 :: Word8)
        B.put i
    put (ConstantExpression c) = do
        B.put (1 :: Word8)
        B.put c
    put (OperatorExpression o) = do
        B.put (2 :: Word8)
        B.put o
    put (IfThenElseExpression i) = do
        B.put (3 :: Word8)
        B.put i
    put (LambdaDefExpression l) = do
        B.put (4 :: Word8)
        B.put l
    put (BracesExpression b) = do
        B.put (5 :: Word8)
        B.put b
    put (VariableExpression v) = do
        B.put (6 :: Word8)
        B.put v
    get = do
        t <- B.get :: B.Get Word8
        case t of 
            0 -> do
                i <- B.get
                return $ InvocationExpression i
            1 -> do
                c <- B.get
                return $ ConstantExpression c
            2 -> do
                o <- B.get
                return $ OperatorExpression o
            3 -> do
                i <- B.get
                return $ IfThenElseExpression i
            4 -> do
                l <- B.get
                return $ LambdaDefExpression l
            5 -> do
                b <- B.get
                return $ BracesExpression b
            _ -> do
                v <- B.get
                return $ VariableExpression v

data Constant = IntConstant Int | StringConstant String | BoolConstant Bool deriving (Show)

instance B.Binary Constant where
    put (IntConstant i) = do
        B.put (0 :: Word8)
        B.put i
    put (StringConstant s) = do
        B.put (1 :: Word8)
        B.put s
    put (BoolConstant b) = do
        B.put (2 :: Word8)
        B.put b
    get = do
        t <- B.get :: B.Get Word8
        case t of 
            0 -> do
                i <- B.get
                return $ IntConstant i
            1 -> do
                s <- B.get
                return $ StringConstant s
            _ -> do
                b <- B.get
                return $ BoolConstant b

data OperatorType = IntType | StringType | BoolType deriving (Show, Eq)

instance B.Binary OperatorType where
    put IntType = B.put (0 :: Word8)
    put StringType = B.put (1 :: Word8)
    put BoolType = B.put (2 :: Word8)
    get = do
        t <- B.get :: B.Get Word8
        case t of 
            0 -> return IntType
            1 -> return StringType
            _ -> return BoolType

data Operator = UnaryOperator OperatorType String Expression
              | BinaryOperator OperatorType String Expression Expression deriving (Show)

instance B.Binary Operator where
    put (UnaryOperator t s e) = do
        B.put (0 :: Word8)
        B.put t
        B.put s
        B.put e
    put (BinaryOperator t s e1 e2) = do
        B.put (1 :: Word8)
        B.put t
        B.put s
        B.put e1
        B.put e2
    get = do
        t <- B.get :: B.Get Word8
        case t of 
            0 -> do
                tp <- B.get
                s <- B.get
                e <- B.get
                return $ UnaryOperator tp s e
            _ -> do
                tp <- B.get
                s <- B.get
                e1 <- B.get
                e2 <- B.get
                return $ BinaryOperator tp s e1 e2

data IfThenElse = IfThenElse Expression Expression Expression deriving (Show)

instance B.Binary IfThenElse where
    put (IfThenElse c e1 e2) = do
        B.put c
        B.put e1
        B.put e2
    get = do
        c <- B.get
        e1 <- B.get
        e2 <- B.get
        return $ IfThenElse c e1 e2

data LambdaDef = LambdaDef [String] Program deriving (Show)

instance B.Binary LambdaDef where
    put (LambdaDef as b) = do
        B.put as
        B.put b
    get = do
        as <- B.get
        b <- B.get
        return $ LambdaDef as b

data ReturnStatement = ReturnStatement Expression deriving (Show)

instance B.Binary ReturnStatement where
    put (ReturnStatement e) = do
        B.put e
    get = do
        e <- B.get
        return $ ReturnStatement e
