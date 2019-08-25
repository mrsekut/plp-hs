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
    reify (Sub e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    sub rax, rdi"
               , "    push rax"
               ]
    reify (Mul e1 e2) =
        reify e1
            ++ reify e2
            ++ ["    pop rdi", "    pop rax", "    imul rdi", "    push rax"]
    reify (Div e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    cqo"
               , "    idiv rdi"
               , "    push rax"
               ]
    reify (Eq e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    cmp rax, rdi"
               , "    sete al"
               , "    movzb rax, al"
               , "    push rax"
               ]
    reify (Neq e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    cmp rax, rdi"
               , "    setne al"
               , "    movzb rax, al"
               , "    push rax"
               ]
    reify (Lt e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    cmp rax, rdi"
               , "    setl al"
               , "    movzb rax, al"
               , "    push rax"
               ]
    reify (Lte e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    cmp rax, rdi"
               , "    setle al"
               , "    movzb rax, al"
               , "    push rax"
               ]
    reify (Gt e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    cmp rax, rdi"
               , "    setg al"
               , "    movzb rax, al"
               , "    push rax"
               ]
    reify (Gte e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    cmp rax, rdi"
               , "    setge al"
               , "    movzb rax, al"
               , "    push rax"
               ]


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
