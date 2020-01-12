import           Text.Parsec
import           Control.Applicative            ( (<$>)
                                                , (*>)
                                                , (<*)
                                                )
import           Text.Parsec.String

data Factor = Land Factor Factor
            | Lor Factor Factor
            | Lnot Factor
            | Nat Int
            deriving Show

-- F -> T F'
-- F' -> v T F' | ε
f :: Parser Factor
f = t `chainl1` (Lor <$ char '∨')


-- T -> L T'
-- T' -> ∧ L T' | ε
t :: Parser Factor
t = l `chainl1` (Land <$ char '∧')


-- L -> ¬L | (F) | i
l :: Parser Factor
l = Lnot <$> (char '¬' *>) l <|> char '(' *> f <* char ')' <|> i

i :: Parser Factor
i = Nat . read <$> many1 digit


run :: Show a => Parser a -> String -> IO ()
run p input = case parse p "" input of
    Left  err -> putStr "parse error at" >> print err
    Right x   -> print x


-- run
-- > run f "1∨1"
-- > run f "1∧1"
-- >  run f "2∨2∧3"
-- Lor (Nat 2) (Land (Nat 2) (Nat 3))
