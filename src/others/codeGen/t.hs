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

eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)             -- Envの中から該当する変数名を探す。lookupはMaybeを返すのでfromJustで取り出す
eval0 env (Plus e1 e2) =
    let IntVal i1 = eval0 env e1
        IntVal i2 = eval0 env e2
    in  IntVal (i1 + i2)
eval0 env (Abs n e) = FuncVal env n e
eval0 env (App e1 e2) =
    let val1 = eval0 env e1         -- FuncValのハズ
        val2 = eval0 env e2         -- 引数
    in  case val1 of
            FuncVal env' n body -> eval0 (Map.insert n val2 env') body       -- 仮引数に実引数を入れて適用


-- 12 + ((λx -> x)(4+2))
exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
-- > eval0 Map.empty exampleExp
-- IntVal 18
