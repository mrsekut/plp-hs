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


type Eval2 a = ErrorT String Identity a
runEval2 :: Eval2 a -> Either String a          -- エラーメッセージと結果
runEval2 ev = runIdentity (runErrorT ev)


eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) = case Map.lookup n env of
    Nothing  -> throwError ("unbound variable: " ++ n)
    Just val -> return val
eval2 env (Plus e1 e2) = do
    e1' <- eval2 env e1
    e2' <- eval2 env e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _                      -> throwError "type error in additoin"
eval2 env (Abs n  e ) = return $ FuncVal env n e
eval2 env (App e1 e2) = do
    val1 <- eval2 env e1         -- FuncValのハズ
    val2 <- eval2 env e2         -- 引数
    case val1 of
        FuncVal env' n body -> eval2 (Map.insert n val2 env') body       -- 仮引数に実引数を入れて適用
        _                   -> throwError "type error in application"



-- 12 + ((λx -> x)(4+2))
exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

-- > runEval2 (eval2 Map.empty (Plus (Lit 1) (Abs "x" (Var "x"))))
-- Left "type error"
-- > runEval2 (eval2 Map.empty (Var "x"))
-- Left "undeifned variable: x"
