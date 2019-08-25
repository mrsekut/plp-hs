module C where

import           Text.Parsec
-- import           Data.List                      ( intercalate )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

data Expr = Add Expr Expr       -- 1 + 2
          | Sub Expr Expr       -- 1 - 2
          | Mul Expr Expr       -- 1 * 2
          | Div Expr Expr       -- 1 / 2
          | Eq Expr Expr        -- 1 == 2
          | Neq Expr Expr       -- 1 == 2
          | Lt Expr Expr        -- 1 < 2
          | Gt Expr Expr        -- 1 > 2
          | Lte Expr Expr       -- 1 <= 2
          | Gte Expr Expr       -- 1 >= 2
          | Nat Int             -- 1,2,..
            deriving Show



class Reifiable a where
    reify :: a -> [ String ]

instance Reifiable Expr where
    reify (Nat i) = [concat ["    push ", show i]]
    reify (Add e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    add rax, rdi"
               , "    push rax"
               ]






gen2 :: Expr -> IO ()
gen2 e = case e of
    Nat i     -> putStrLn $ "    push " ++ show i
    Add e1 e2 -> do
        gen2 e1
        gen2 e2
        putStrLn "    pop rdi"
        putStrLn "    pop rax"
        putStrLn "    add rax, rdi"
        putStrLn "    push rax"
    -- Sub e1 e2 -> do
    --     gen2 e1
    --     gen2 e2
    --     putStrLn "    pop rdi"
    --     putStrLn "    pop rax"
    --     putStrLn "    sub rax, rdi"
    --     putStrLn "    push rax"
    -- Mul e1 e2 -> do
    --     gen2 e1
    --     gen2 e2
    --     putStrLn "    pop rdi"
    --     putStrLn "    pop rax"
    --     putStrLn "    imul rdi"
    --     putStrLn "    push rax"
    -- Div e1 e2 -> do
    --     gen2 e1
    --     gen2 e2
    --     putStrLn "    pop rdi"
    --     putStrLn "    pop rax"
    --     putStrLn "    cqo"
    --     putStrLn "    idiv rdi"
    --     putStrLn "    push rax"
    -- Eq e1 e2 -> do
    --     gen2 e1
    --     gen2 e2
    --     putStrLn "    pop rdi"
    --     putStrLn "    pop rax"
    --     putStrLn "    cmp rax, rdi"
    --     putStrLn "    sete al"
    --     putStrLn "    movzb rax, al"
    --     putStrLn "    push rax"
    -- Neq e1 e2 -> do
    --     gen2 e1
    --     gen2 e2
    --     putStrLn "    pop rdi"
    --     putStrLn "    pop rax"
    --     putStrLn "    cmp rax, rdi"
    --     putStrLn "    setne al"
    --     putStrLn "    movzb rax, al"
    --     putStrLn "    push rax"
    -- Lt e1 e2 -> do
    --     gen2 e1
    --     gen2 e2
    --     putStrLn "    pop rdi"
    --     putStrLn "    pop rax"
    --     putStrLn "    cmp rax, rdi"
    --     putStrLn "    setl al"
    --     putStrLn "    movzb rax, al"
    --     putStrLn "    push rax"
    -- Lte e1 e2 -> do
    --     gen2 e1
    --     gen2 e2
    --     putStrLn "    pop rdi"
    --     putStrLn "    pop rax"
    --     putStrLn "    cmp rax, rdi"
    --     putStrLn "    setle al"
    --     putStrLn "    movzb rax, al"
    --     putStrLn "    push rax"
    -- Gt e1 e2 -> do
    --     gen2 e1
    --     gen2 e2
    --     putStrLn "    pop rdi"
    --     putStrLn "    pop rax"
    --     putStrLn "    cmp rax, rdi"
    --     putStrLn "    setg al"
    --     putStrLn "    movzb rax, al"
    --     putStrLn "    push rax"
    -- Gte e1 e2 -> do
    --     gen2 e1
    --     gen2 e2
    --     putStrLn "    pop rdi"
    --     putStrLn "    pop rax"
    --     putStrLn "    cmp rax, rdi"
    --     putStrLn "    setge al"
    --     putStrLn "    movzb rax, al"
    --     putStrLn "    push rax"


gen :: Expr -> IO ()
gen = mapM_ putStrLn . reify


asmHeader = mapM_ putStrLn [".intel_syntax noprefix", ".global hcc", "hcc:"]
main :: IO ()
main = do
    let p = Add (Nat 2) (Nat 3)
    asmHeader
    gen p
    putStrLn $ "    pop rax"
    putStrLn $ "    ret"
