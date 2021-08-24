module Syntax where

import Data.Function (on)

type Name = String
type Assignment = (Name,Expr)

symbolize :: String -> Name
symbolize = id

data Type = Type Name | Fun Type Type deriving (Eq,Ord)
type Typing = Maybe Type

data Expr
  = App Expr Expr Typing
  | Lam Name Expr Typing
  | Lit Name Typing
  deriving (Eq,Ord)

app r l = App r l Nothing
lam v b = Lam v b Nothing
lit c = Lit c Nothing

anyType = Type "*"

unjust' :: Maybe a -> a
unjust' (Just x) = x
unjust (Just x) = x
unjust Nothing = anyType

codomain :: Type -> Type
codomain (Fun _ b) = b
codomain t = t
domain :: Type -> Type
domain (Fun a _) = a
domain t = t

assertType (App r l _) t = App r l (Just t)
assertType (Lam v b _) t = Lam v b (Just t)
assertType (Lit c _) t   = Lit c (Just t)


instance Show Type where
  show (Type c)  = c
  show (Fun a b) = "(" ++ show a ++ "->" ++ show b ++ ")"

showTyping (Just t) = ':':show t
showTyping Nothing = showTyping (Just anyType)

lamParam p = "\\" ++ p ++ "." -- "λ" ++ p ++ "." -- replit sux

instance Show Expr where
  show (App l r t) = show l ++ " " ++ show r
  show (Lam v b t) = "(" ++ lamParam (v ++ ':':show (domain $ unjust $ t)) ++ show b ++ ")" ++ showTyping t
  show (Lit v t)   = v ++ showTyping t

showTypeless (App l r _) = on (\a b -> "(" ++ a ++ " " ++ b ++ ")") showTypeless l r
showTypeless (Lam v b _) = "(" ++ lamParam v ++ showTypeless b ++ ")"
showTypeless (Lit v _)   = v

showLisp (App l r _) = on (\a b -> "(" ++ a ++ " " ++ b ++ ")") showLisp l r
showLisp (Lam v b _) = "(lambda ("++v++") "++showLisp b++")"
showLisp (Lit v _)   = v