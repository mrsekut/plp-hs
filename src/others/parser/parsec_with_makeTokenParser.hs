-- ref: http://m12i.hatenablog.com/entry/2013/11/13/023128
module Parsec2 where

import           Control.Applicative            ( (<*) )
import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Expr
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )

-- 文法規則
-- stmt ::= nop | var := expr | if expr then stmt else stmt fi | while expr do stmt od | stmt { ; stmt }+
-- expr  ::= var | const | ( expr ) | unop expr | expr duop expr
-- var   ::= letter { letter | digit }*
-- const ::= true | false
-- unop  ::= ~
-- duop  ::= & | =

data Stmt = Nop | String := Expr | If Expr Stmt Stmt | While Expr Stmt | Seq [Stmt] deriving Show
data Expr = Var String | Con Bool | Uno Unop Expr | Duo Duop Expr Expr deriving Show
data Unop = Not deriving Show
data Duop = And | Iff deriving Show

-- emptyDefを拡張
def = emptyDef
  { commentStart    = "{-"
  , commentEnd      = "-}"
  , identStart      = letter
  -- , identLetter     = alphaNum -- 何故かあるとerrorになる
  , opStart         = oneOf "~&=:"
  , opLetter        = oneOf "~&=:"
  , reservedOpNames = ["~", "&", "=", ":="]
  , reservedNames   = [ "true"
                      , "false"
                      , "nop"
                      , "if"
                      , "then"
                      , "else"
                      , "fi"
                      , "while"
                      , "do"
                      , "od"
                      ]
  }

-- Token Parserの作成
TokenParser { parens = m_parens, identifier = m_identifier, reservedOp = m_reservedOp, reserved = m_reserved, semiSep1 = m_semiSep1, whiteSpace = m_whiteSpace }
  = makeTokenParser def

-- ex. --------------------------
--
-- 丸括弧をパースし、その中の文字列をパースする
-- parseTest (m_parens m_identifier) "(hoge)"
-- > "hoge"
--
-- 識別子
-- parseTest m_identifier "hoge"
-- > "hoge"
-- 予約語と一致するとエラー
-- parseTest m_identifier "if"
-- > error
--
-- 第一引数に指定した演算子と一致するかを判定
-- parseTest (m_reservedOp ":=") ":="
-- > ()
--
-- 第一引数に指定した予約語と一致するかを判定
-- parseTest (m_reserved "if") "if"
-- > ()
--
-- セミコロンで分割されたワードを第一引数の関数でパース
-- parseTest (m_semiSep1  m_identifier ) "hoge; piyo; foo"
-- > ["hoge","piyo","foo"]
--
-- ホワイトスペースをスキップ
-- parseTest m_whiteSpace "  "
-- ()
---------------------------------

-- Expression Parserの作成
exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table =
  [ [Prefix (m_reservedOp "~" >> return (Uno Not))]
  , [Infix (m_reservedOp "&" >> return (Duo And)) AssocLeft]
  , [Infix (m_reservedOp "=" >> return (Duo Iff)) AssocLeft]
  ]

term =
  m_parens exprparser
    <|> fmap Var m_identifier
    <|> (m_reserved "true" >> return (Con True))
    <|> (m_reserved "false" >> return (Con False))


-- ex. --------------------------
--
-- parseTest exprparser "hoge & piyo"
-- > Duo And (Var "hoge") (Var "piyo")
--
-- parseTest exprparser "t = p"
-- > Duo Iff (Var "t") (Var "p")
--
-- parseTest exprparser "true hoge"
-- > Con True
---------------------------------
