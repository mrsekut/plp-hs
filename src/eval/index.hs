-- STRefを使ったevalの練習
import           Data.IORef
import           Data.Maybe



data AST = Int Int
         | Plus AST AST
         | Var String
         | Assign String AST
        deriving (Eq, Show)

type Env = IORef [(String, IORef AST)]



eval :: AST -> Env -> IO Int
eval (Int n     ) _   = return n
eval (Plus x1 x2) env = do
    n1 <- eval x1 env
    n2 <- eval x2 env
    return $ n1 + n2
eval (Var x) env = do
    v <- getVar x env
    eval v env
eval (Assign var ast) env = do
    envBind var ast env
    eval ast env



{- 試す -}

-- 変数の束縛と参照
useBindVar :: IO ()
useBindVar = do
    env1 <- emptyEnv
    print =<< eval (Assign "error" (Int (-1))) env1

    print =<< eval (Assign "x" (Int 42)) env1 -- 'y = 42' >> 42
    print =<< eval (Assign "y" (Plus (Int 3) (Int 5))) env1 -- 'x = 3 + 5' >> 8
    print =<< eval (Var "y") env1 -- 'y' >> 8
    print =<< eval (Plus (Var "y") (Int 5)) env1 -- `y + 5` >> 13

main = useBindVar




{- Utils -}


emptyEnv :: IO Env
emptyEnv = newIORef []


getVar :: String -> Env -> IO AST
getVar var env = do
    e <- readIORef env
    case lookup var e of
        Just v  -> readIORef v
        Nothing -> return (Var "error") -- error


envBind :: String -> AST -> Env -> IO AST
envBind var ast env = do
    hasVar <- isBound var env
    if hasVar
        then setVar var ast env >> return ast
        else do
            a <- newIORef ast
            e <- readIORef env
            writeIORef env ((var, a) : e)
            return ast

  where
    isBound :: String -> Env -> IO Bool
    isBound var env = isJust . lookup var <$> readIORef env

    setVar :: String -> AST -> Env -> IO AST
    setVar var ast env = do
        e <- readIORef env
        case lookup var e of
            Just v  -> writeIORef v ast
            Nothing -> return ()
        return ast
