-- ref: http://m12i.hatenablog.com/entry/2013/11/13/023128
module Parsec2 where

import           Control.Applicative            ( (<*) )
import           Text.Parsec                    ( alphaNum
                                                , oneOf
                                                , letter
                                                )
-- import           Text.Parsec.String
-- import           Text.Parsec.Expr
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
  , identLetter     = alphaNum
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
TokenParser { parens = m_parens           -- 開き括弧
            , identifier = m_identifier   -- 識別子
            , reservedOp = m_reservedOp   -- 予約した演算子かチェック
            , reserved = m_reserved       -- 予約語かチェック
            , semiSep1 = m_semiSep1       -- セミコロン
            , whiteSpace = m_whiteSpace   -- skip whiteSpaces
            }
  = makeTokenParser def