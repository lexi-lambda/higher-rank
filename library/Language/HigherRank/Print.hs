module Language.HigherRank.Print (printExpr, printReducedExpr, printType) where

import Language.HigherRank.Types

printExpr :: Expr -> String
printExpr EUnit = "()"
printExpr (EVar (MkEVar x)) = x
printExpr (EAnn e t) = "(" ++ printExpr e ++ " : " ++ printType t ++ ")"
printExpr (ELam (MkEVar x) e) = "(\\" ++ x ++ " -> " ++ printExpr e ++ ")"
printExpr (EApp a b) = "(" ++ printExpr a ++ " " ++ printExpr b ++ ")"

printReducedExpr :: ReducedExpr -> String
printReducedExpr REUnit = "()"
printReducedExpr (RETuple x y) = "(" ++ printReducedExpr x ++ ", " ++ printReducedExpr y ++ ")"
printReducedExpr (RELeft x) = "(" ++ printReducedExpr x ++ " |)"
printReducedExpr (RERight x) = "(| " ++ printReducedExpr x ++ ")"
printReducedExpr (RELam _ (MkEVar x) e) = "(\\" ++ x ++ " -> " ++ printExpr e ++ ")"
printReducedExpr (REPrim op) = "#<prim:" ++ primOpName op ++ ">"

printType :: Type -> String
printType TUnit = "()"
printType (TProduct x y) = "(" ++ printType x ++ ", " ++ printType y ++ ")"
printType (TSum x y) = "(" ++ printType x ++ " | " ++ printType y ++ ")"
printType (TVar (MkTVar x)) = x
printType (TEVar (MkTEVar x)) = x ++ "'"
printType (TArr a b) = "(" ++ printType a ++ " -> " ++ printType b ++ ")"
printType (TAll (MkTVar v) a) = "(forall " ++ v ++ ". " ++ printType a ++ ")"
