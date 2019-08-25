-- ref: http://bicycle1885.hatenablog.com/entry/2012/12/08/165236
module Transformers where

import           Control.Monad.Identity
import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Maybe
import qualified Data.Map                      as Map


type Name = String
data Exp = Lit Integer
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp             -- λ式
         | App Exp Exp              -- 関数適用
         deriving (Show)
data Value = IntVal Integer         -- 整数
           | FuncVal Env Name Exp   -- 関数
           deriving (Show)
type Env = Map.Map Name Value


type Eval5 a
    = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer Identity))) a
runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env st ev =
    runIdentity (runStateT (runWriterT (runErrorT (runReaderT ev env))) st)

eval5 :: Exp -> Eval5 Value
eval5 (Lit i) = do
    tick
    return $ IntVal i
eval5 (Var n) = do
    tick
    tell [n]
    env <- ask
    case Map.lookup n env of
        Nothing  -> throwError ("unbound variable: " ++ n)
        Just val -> return val
eval5 (Plus e1 e2) = do
    tick
    e1' <- eval5 e1
    e2' <- eval5 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _                      -> throwError "type error in additoin"
eval5 (Abs n e) = do
    tick
    env <- ask
    return $ FuncVal env n e
eval5 (App e1 e2) = do
    tick
    val1 <- eval5 e1         -- FuncValのハズ
    val2 <- eval5 e2         -- 引数
    case val1 of
        FuncVal env' n body ->
            local (const (Map.insert n val2 env')) (eval5 body)       -- 仮引数に実引数を入れて適用
        _ -> throwError "type error in application"


tick :: (Num s, MonadState s m) => m ()
tick = do
    st <- get
    put (st + 1)

-- 12 + ((λx -> x)(4+2))
exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
-- > runEval5 Map.empty 0 (eval5 exampleExp)
-- ((Right (IntVal 18),["x"]),8)

-- ((λx -> (λy -> x + y))(4)(2))
otherExampleExp =
    App (App (Abs "x" (Abs "y" (Plus (Var "x") (Var "y")))) (Lit 4)) (Lit 2)
-- > runEval5 Map.empty 0 (eval5 otherExampleExp)
-- ((Right (IntVal 6),["x","y"]),9)
