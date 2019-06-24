-- TaPL 4
-- repository作ろう
-- - ちゃんとプロジェクト作る
-- 以下は書きかけ。モナドのところがわからｎ

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          deriving Show

isNumericVal :: Term -> Bool
isNumericVal TmZero = True
isNumericVal TmSucc t1 = isNumericVal t1
isNumericVal _ = False

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal t = isNumericVal t
isVal _ = False

eval1 :: Term -> Maybe Bool
eval1 TmIf TmTrue  t2 _ = t2
eval1 TmIf TmFalse _ t3 = t3
eval1 TmIf fi t1 t2 t3
    where t1' = eval1 t1
eval1 TmSucc
eval1 TmPred = Just TmZero
eval1 TmPred
eval1 TmPred
eval1 TmIsZero = Just TmTrue
eval1 TmIsZero
eval1 TmIsZero
eval1 _ = Nothing
