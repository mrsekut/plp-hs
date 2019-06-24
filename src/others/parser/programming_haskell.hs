-- 「プログラミングHaskell」8章
import           Data.Char                      ( isDigit )

-- []のときerrorを表現
-- 返り値のStringは消費しなかったあまりの文字列
type Parser a = String -> [(a, String)]

--解析が必ず成功し、入力文字を消費しない
rtn :: a -> Parser a
rtn v = \inp -> [(v, inp)]

-- 入力文字に関わらず失敗する
failure :: Parser a
failure = \inp -> []

item :: Parser Char
item []       = []
item (x : xs) = [(x, xs)]


parse :: Parser a -> String -> [(a, String)]
parse p xs = p xs


-- 連結
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \inp -> case parse p inp of
    []         -> []
    [(v, out)] -> parse (f v) out


-- p :: Parser (Char, Char)
-- p = do
--     v1 <- tmp
--     tmp
--     v3 <- tmp
--     return [(v1, v3)]

-- 選択
-- 一番目のparserが失敗したら二番目のparserを適用する
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case parse p inp of
    []         -> parse q inp
    [(v, out)] -> [(v, out)]

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isLetter

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []       = return []
string (x : xs) = do
    char x
    string xs
    return (x : xs)

-- 途中
