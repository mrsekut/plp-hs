-- 俺々言語にも型推論が欲しい
import qualified Data.Map as M
import qualified Data.Set as S

-- 項
data Term
    = ValueVariable VariableName
    | IntegerValue Int
    | BooleanValue Bool
    | Plus Term Term
    | If Term Term Term
    | Abstract VariableName Term
    | Application Term Term
    deriving (Show, Eq)

-- 型
data Type
    = TypeVariable VariableName
    | Integer
    | Boolean
    | Arrow Type Type
    deriving (Show, Eq, Ord)

type Context = M.Map VariableName Type
type Constraint = S.Set (Type, Type)
type Assign = (VariableName, Type)
type VariableName = String


-- Abstract "y" (Plus (ValueVariable "x") (ValueVariable "y"))
-- Arrow  (TypeVariable "X") (Arrow Integer Boolean)
-- Data.Map.singleton "f" (Arrow Integer Integer)
-- Data.Set.singleton ("X", Boolean)

constraintType :: Context -> Term -> [VariableName] -> Either String (Type, Constraint, [VariableName])
constraintType _ (IntegerValue _) names = pure (Integer, S.empty, names)
constraintType _ (BooleanValue _) names = pure (Boolean, S.empty, names)
constraintType ctx (ValueVariable x) names = do
    typ <- case M.lookup x ctx of
        Just typ' -> pure typ'
        Nothing -> Left ("the context (" <> show (M.toList ctx) <> ") doesn't have type variable (" <> x <> ")")
    pure (typ, S.empty, names)

constraintType ctx (Plus t1 t2) names = do
    (typ1, cst1, names1) <- constraintType ctx t1 names
    (typ2, cst2, names2) <- constraintType ctx t2 names1
    let cst = S.unions [cst1, cst2, S.fromList [(typ1, Integer), (typ2, Integer)]]
    pure (Integer, cst, names2)
constraintType ctx (If t1 t2 t3) names = do
    (typ1, cst1, names1) <- constraintType ctx t1 names
    (typ2, cst2, names2) <- constraintType ctx t2 names1
    (typ3, cst3, names3) <- constraintType ctx t3 names2
    let cst = S.unions [cst1, cst2, cst3, S.fromList [(typ1, Boolean), (typ2, typ3)]]
    pure (typ2, cst, names3)
constraintType ctx (Abstract x t1) names = do
    (xt, names0) <- newTypeVariable names
    let ctx0 = M.insert x xt ctx
    (typ1, cst1, names1) <- constraintType ctx0 t1 names0
    pure (Arrow xt typ1, cst1, names1)
constraintType ctx (Application t1 t2) names = do
    (typ1, cst1, names1) <- constraintType ctx t1 names
    (typ2, cst2, names2) <- constraintType ctx t2 names1
    (typ, names3) <- newTypeVariable names2
    let cst = S.unions [cst1, cst2, S.singleton (typ1, Arrow typ2 typ)]
    pure (typ, cst, names3)

newTypeVariable :: [VariableName] -> Either String (Type, [VariableName])
newTypeVariable (n:names) = pure (TypeVariable n, names)
newTypeVariable []        = Left "no unused type variables"