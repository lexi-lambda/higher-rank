module Language.HigherRank.Print (printExpr, printType) where

import Language.HigherRank.Typecheck (EVar(..), Expr(..), TEVar(..), TVar(..), Type(..))

printExpr :: Expr -> String
printExpr EUnit = "()"
printExpr (EVar (MkEVar x)) = x
printExpr (EAnn e t) = "(" ++ printExpr e ++ " : " ++ printType t ++ ")"
printExpr (ELam (MkEVar x) e) = "(\\" ++ x ++ " -> " ++ printExpr e ++ ")"
printExpr (EApp a b) = "(" ++ printExpr a ++ " " ++ printExpr b ++ ")"

printType :: Type -> String
printType TUnit = "()"
printType (TVar (MkTVar x)) = x
printType (TEVar (MkTEVar x)) = x ++ "'"
printType (TArr a b) = "(" ++ printType a ++ " -> " ++ printType b ++ ")"
printType (TAll (MkTVar v) a) = "(forall " ++ v ++ ". " ++ printType a ++ ")"
