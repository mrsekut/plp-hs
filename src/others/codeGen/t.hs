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


type Eval3 a = ReaderT Env (ErrorT String Identity) a
runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity (runErrorT (runReaderT ev env))


eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do
    env <- ask                                                  -- askってなんぞ
    case Map.lookup n env of
        Nothing  -> throwError ("unbound variable: " ++ n)
        Just val -> return val
eval3 (Plus e1 e2) = do
    e1' <- eval3 e1
    e2' <- eval3 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _                      -> throwError "type error in additoin"
eval3 (Abs n e) = do
    env <- ask
    return $ FuncVal env n e
eval3 (App e1 e2) = do
    val1 <- eval3 e1         -- FuncValのハズ
    val2 <- eval3 e2         -- 引数
    case val1 of
        FuncVal env' n body ->
            local (const (Map.insert n val2 env')) (eval3 body)       -- 仮引数に実引数を入れて適用
        _ -> throwError "type error in application"


-- 12 + ((λx -> x)(4+2))
exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

-- > runEval3 Map.empty (eval3 exampleExp)
-- Right (IntVal 18)