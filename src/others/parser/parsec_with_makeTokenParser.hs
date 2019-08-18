-- ref: http://m12i.hatenablog.com/entry/2013/11/13/023128
module ParsecWithMakeTokenParser where

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


-- 型の定義
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


-- Statement Parserの作成
-- Token ParserとExpression Parserを組み合わせて再帰下降を作る
mainparser :: Parser Stmt
mainparser = m_whiteSpace >> stmtparser <* eof
 where
  stmtparser :: Parser Stmt
  stmtparser = fmap Seq (m_semiSep1 stmt1)
  stmt1 =
    (m_reserved "nop" >> return Nop)
      <|> do
            v <- m_identifier
            m_reservedOp ":="
            e <- exprparser
            return (v := e)
      <|> do
            m_reserved "if"
            b <- exprparser
            m_reserved "then"
            p <- stmtparser
            m_reserved "else"
            q <- stmtparser
            m_reserved "fi"
            return (If b p q)
      <|> do
            m_reserved "while"
            b <- exprparser
            m_reserved "do"
            p <- stmtparser
            m_reserved "od"
            return (While b p)


-- use

play :: String -> IO ()
play inp = case parse mainparser "" inp of
  Left  err -> print err
  Right ans -> print ans


-- ex. --------------------------
--
--  play "h := hoge"
-- > Seq ["h" := Var "hoge"]
--
-- play "if true then a := b else a := b fi"
-- > Seq [If (Con True) (Seq ["a" := Var "b"]) (Seq ["a" := Var "b"])]
--
-- play "while true do x := hoge od"
-- > Seq [While (Con True) (Seq ["x" := Var "hoge"])]
---------------------------------
