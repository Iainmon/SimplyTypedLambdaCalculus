module Typing where

import Syntax
import Data.Function



typePrecidence :: Type -> Type -> Type
typePrecidence a b | not $ a == anyType = a
                   | otherwise        = b


findFirstBoundVarType :: Name -> Expr -> Type
findFirstBoundVarType c (Lit v t)   = if c == v then typeof (Lit v t) else anyType
findFirstBoundVarType c (App r l _) = on typePrecidence (findFirstBoundVarType c) r l
findFirstBoundVarType c (Lam v body _) = if v == c then anyType else findFirstBoundVarType c body

typeof :: Expr -> Type
typeof (Lit c Nothing) = anyType
typeof (Lam v body Nothing) = Fun (findFirstBoundVarType v body) (typeof body)
typeof (App l r Nothing) = codomain $ typeof l
typeof (Lit _ t)   = unjust' t
typeof (Lam _ _ t) = unjust' t
typeof (App _ _ t) = unjust' t


solveTypes :: Expr -> Expr
solveTypes e@(Lit c t)   = Lit c (Just $ typeof e)
solveTypes e@(Lam v b t) = Lam v (solveTypes b) (Just $ typeof e)
solveTypes e@(App l r t) = on (\a b -> App a b (Just $ typeof e)) solveTypes l r


typeCheck' :: Expr -> Maybe Expr
typeCheck' (Lam v b t) = if (codomain . unjust) t == typeof b
                           then typeCheck' b >>= return . flip (Lam v) t
                           else Nothing
typeCheck' (App l r t) = if or (map ((domain . typeof) l ==) [typeof r,anyType]) && or (map ((codomain . typeof) l ==) [unjust t,anyType]) then (\a b -> App a b t) <$> typeCheck' l <*> typeCheck' r else Nothing
typeCheck' lit' = Just lit'

typeCheck :: Expr -> Maybe Expr
typeCheck = typeCheck' . solveTypes