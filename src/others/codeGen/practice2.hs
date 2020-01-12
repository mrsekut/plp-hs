{-# LANGUAGE OverloadedStrings,GADTs,FlexibleInstances #-}

import           Data.List                      ( intercalate )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

type Ident = Text

-- 2D matrix
data Mat2D r = Mat Ident | MatConst [[r]]

class Scalar a
instance Scalar Int
instance Scalar Integer
instance Scalar Double

data Exp r where
    Const ::(Reifiable r) => r -> Exp r
    Add ::Exp r -> Exp r -> Exp r
    Sub ::Exp r -> Exp r -> Exp r
    Mul ::Exp r -> Exp r -> Exp r
    Abs ::(Scalar r) => Exp r -> Exp r
    Signum ::(Scalar r) => Exp r -> Exp r
    Index2D ::(Num r) => Exp Int -> Exp Int -> Exp (Mat2D r) -> Exp r

-- instance (Scalar r) => Num (Exp (Mat2D r)) where
--     a + b = Add a b
--     a - b = Sub a b
--     a * b = Mul a b
--         -- Caution! abs, signum, fromInteger are not defined.

-- instance Num (Exp Int) where
--     a + b = Add a b
--     a - b = Sub a b
--     a * b = Mul a b
--     fromInteger a = Const (fromIntegral a)
--         -- Caution! abs and signum are not defined.

-- instance Num (Exp Double) where
--     a + b = Add a b
--     a - b = Sub a b
--     a * b = Mul a b
--     fromInteger a = Const (fromIntegral a)
--         -- Caution! abs and signum are not defined.

-- instance Num (Exp Integer) where
--     a + b = Add a b
--     a - b = Sub a b
--     a * b = Mul a b
--     fromInteger a = Const (fromIntegral a)
--         -- Caution! abs and signum are not defined.

class Reifiable a where
        reify :: a -> Text

instance Reifiable (Exp r) where
    reify (Const exp) = reify exp
    reify (Add e1 e2) = T.concat [reify e1, "+", reify e2]
    reify (Index2D i j mat) =
        T.concat [reify mat, "(", reify i, ",", reify j, ")"]

instance (Show r) => Reifiable (Mat2D r) where
    reify (Mat ident) = ident
    reify (MatConst xss) =
        T.pack
            $  "["
            ++ intercalate ";" (map (intercalate " " . map show) xss)
            ++ "]"

instance Reifiable Int where
    reify n = T.pack $ show n

instance Reifiable Integer where
    reify n = T.pack $ show n

instance Reifiable Double where
    reify n = T.pack $ show n

main = do
    gen m1
    -- gen test2
    -- gen test3
    -- gen test4
    -- gen test5

gen :: Exp r -> IO ()
gen = T.putStrLn . reify

m1 :: Exp (Mat2D Int)
m1 = Const (MatConst [[1, 2, 3], [4, 5, 6]])

-- m2 :: Exp (Mat2D Int)
-- m2 = Const (MatConst [[10, 5, 3], [1, 4, 2]])

-- test4 :: Exp (Mat2D Int)
-- test4 = m1 + m1

-- test5 :: Exp (Mat2D Int)
-- test5 = foldl1 (+) (replicate 5 m1)

-- test2 :: Exp Int
-- test2 = Index2D 1 2 m1

-- test3 :: Exp Int
-- test3 = Index2D (Index2D 1 2 m1) 1 m2

{- Examples
*Main> main
[1 2 3;4 5 6]
[1 2 3;4 5 6](1,2)
[10 5 3;1 4 2]([1 2 3;4 5 6](1,2),1)
[1 2 3;4 5 6]+[1 2 3;4 5 6]
[1 2 3;4 5 6]+[1 2 3;4 5 6]+[1 2 3;4 5 6]+[1 2 3;4 5 6]+[1 2 3;4 5 6]
-}
