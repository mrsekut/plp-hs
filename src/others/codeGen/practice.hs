-- ref: https://keigoi.hatenadiary.org/entry/20111206/haskell_tagless_dsl
module Gen where

import           Control.Monad.State

-- 式, lは変換先の型を表す
newtype Exp l a = Exp {unExp :: l a}

-- 関数, 変換先言語での演算子名か関数名
data FunName = Op String | Name String
newtype Func1 a b = Func1 (FunName, a -> b)         -- C言語のコード生成時に使う
newtype Func2 a b c = Func2 (FunName, a -> b -> c)  -- Haskellの式の評価時に使う

class Lang (l :: * -> *) where
    true :: Exp l Bool
    false :: Exp l Bool
    int :: Int -> Exp l Int     -- なんで関数？数値の型は色々あるからか？
    func1 :: Func1 a b -> Exp l a -> Exp l b
    func2 :: Func2 a b c -> Exp l a -> Exp l b -> Exp l c
    ifThenElse :: Exp l Bool -> Exp l Int -> Exp l Int -> Exp l Int -- なんで一階層深い？
    -- loop -> from -> to -> (acc) -> fの形になっている
    loop :: Exp l Int -> Exp l Int -> Exp l Int -> (Exp l Int -> Exp l Int -> Exp l Int) -> Exp l Int
    let_ :: Exp l Int -> (exp l Int -> Exp l Int) -> Exp l Int


-- 名前生成Monad
class Monad q => MonadQ q where
    newName :: String -> q String

instance MonadQ (State [(String, Int)]) where
    newName str = do
        varmap <- get
        let cnt = fromMaybe 0 $ lookup str varmap
        put
            $ (str, cnt + 1)
            : deleteBy (\(a, _) (b, _) -> a == b) (str, 0) varmap
        return $ str ++ show cnt

instance (Monoid w, MonadQ m) => MonadQ (WriterT w m) where
    newName = lift . newName


type W a = WriterT [C.BlockItem] (State [(String, Int)]) a

newtype CGen a = CGen {unCGen :: W C.Exp}


unExp_ :: Exp CGen a -> W C.Exp
unExp_ (Exp (CGen m)) = m

cgen :: C.Exp -> Exp CGen a
cgen = E . CG . return

cgenW :: W C.Exp -> Exp CGen a
cgenW = E . CG


instance Lang CGen where
    true  = cgen $ [cexp| TRUE|]
    false = cgen $ [cexp| FALSE|]
    int i = cgen $ [cexp| $int:i |]
    func1 fun e1 = cgenW $ do
        x <- unExp_ e1
        return $ case fun of
        Func1(Name funname,_) -> [cexp| $id:funname( $x ) |]
        Func1(Op opname,_) -> (C.UnOp (toCUnOp opname) x noSrcLoc)
    func2 fun e1 e2 = cgenW $ do
        x <- unExp_ e1;
        y <- unExp_ e2;
        return $ case fun of
        Func2(Name funname,_) -> [cexp| $id:funname( $x, $y ) |]
        Func2(Op opname,_) -> (C.BinOp (toCBinOp opname) x y noSrcLoc)
    ifThenElse cond then_ else_ = cgenW $ do
        tmp <- newName "tmp"
        cond' <- unExp_ cond
        (thenExp, thenStmts) <- lift $ runWriterT $ unExp_ then_
        (elseExp, elseStmts) <- lift $ runWriterT $ unExp_ else_
        -- 変数宣言を生成し、if文の内部で代入する
        tell [C.BlockDecl [cdecl|
                int $id:tmp;
            |], C.BlockStm [cstm|
                if($cond') {
                $items:thenStmts
                $id:tmp = $thenExp;
                } else {
                $items:thenStmts
                $id:tmp = $elseExp;
                }
            |] ]
        return [cexp| $id:tmp |]