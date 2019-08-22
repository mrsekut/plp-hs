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

type Eval1 a = Identity a
runEval1 :: Eval1 a -> a
runEval1 ev = runIdentity ev

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) =
    -- lookupはどんなMonadのなかでもfail関数を呼べる
    maybe (fail ("undeifned variable: " ++ n)) return $ Map.lookup n env                     -- Envの中から該当する変数名を探す。lookupはMaybeを返すのでfromJustで取り出す
eval1 env (Plus e1 e2) = do
    IntVal i1 <- eval1 env e1
    IntVal i2 <- eval1 env e2
    return $ IntVal (i1 + i2)
eval1 env (Abs n  e ) = return $ FuncVal env n e
eval1 env (App e1 e2) = do
    val1 <- eval1 env e1         -- FuncValのハズ
    val2 <- eval1 env e2         -- 引数
    case val1 of
        FuncVal env' n body -> eval1 (Map.insert n val2 env') body       -- 仮引数に実引数を入れて適用

-- 12 + ((λx -> x)(4+2))
exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
-- > runEval1 (eval1 Map.empty exampleExp)
-- IntVal 18