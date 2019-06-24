{
    module Lexer where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-

if                                              { \s -> If }
$digit+                                         { \s => Num (read s :: Integer) }
$alpha ($alpha | $digit)                        { \s -> Id }
($digit+"."$digit*) | ($digit*"."$digit+)   { \s -> Real (read s :: Integer) }
("--"$alpha*"\n") | (" "|"\n"|"\t")+            { \s -> Error }

{
--  Each action has type :: String -> Token

-- The token type:
data Token =
    If       |
    Num      |
    Id       |
    Real     |
    Error
    deriving (Eq, Show)

main = do
    s <- getContents
    print (alexScanTOkens s)
}