{
{-# LANGUAGE OverloadedStrings                 #-}
module Main (main) where
}

%wrapper "basic"

$digit = 0-9			-- digits

tokens :-

  $white+				                    ;
  "--".*				                    ;
  $digit+				                    { \s -> TokenInt (read s) }
  [\+]                              { \s -> TokenPlus }
  [\-]                              { \s -> TokenMinus }
  [\*]                              { \s -> TokenMult }
  [\\]                              { \s -> TokenDiv }
  [\(]                              { \s -> TokenLP }
  [\)]                              { \s -> TokenRP }

{
-- The token type:
data Token
  = TokenInt Int
  | TokenPlus
  | TokenMinus
  | TokenMult
  | TokenDiv
  | TokenLP
  | TokenRP
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}