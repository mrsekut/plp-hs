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


type Eval6 a
    = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer IO))) a
runEval6
    :: Env -> Integer -> Eval6 a -> IO ((Either String a, [String]), Integer)
runEval6 env st ev = runStateT (runWriterT (runErrorT (runReaderT ev env))) st

eval6 :: Exp -> Eval6 Value
eval6 (Lit i) = do
    tick
    liftIO $ print i
    return $ IntVal i
eval6 (Var n) = do
    tick
    tell [n]
    env <- ask
    case Map.lookup n env of
        Nothing  -> throwError ("unbound variable: " ++ n)
        Just val -> return val
eval6 (Plus e1 e2) = do
    tick
    e1' <- eval6 e1
    e2' <- eval6 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _                      -> throwError "type error in additoin"
eval6 (Abs n e) = do
    tick
    env <- ask
    return $ FuncVal env n e
eval6 (App e1 e2) = do
    tick
    val1 <- eval6 e1         -- FuncValのハズ
    val2 <- eval6 e2         -- 引数
    case val1 of
        FuncVal env' n body ->
            local (const (Map.insert n val2 env')) (eval6 body)       -- 仮引数に実引数を入れて適用
        _ -> throwError "type error in application"


tick :: (Num s, MonadState s m) => m ()
tick = do
    st <- get
    put (st + 1)

-- 12 + ((λx -> x)(4+2))
exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
-- > runEval6 Map.empty 0 (eval6 exampleExp)


-- ((λx -> (λy -> x + y))(4)(2))
otherExampleExp =
    App (App (Abs "x" (Abs "y" (Plus (Var "x") (Var "y")))) (Lit 4)) (Lit 2)
-- > runEval6 Map.empty 0 (eval6 otherExampleExp)
