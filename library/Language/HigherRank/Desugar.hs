module Language.HigherRank.Desugar (desugar) where

import Language.HigherRank.Types

desugar :: SugaredExpr -> Expr
desugar SEUnit = EUnit
desugar (SETuple x y) = EVar (MkEVar "Tuple") `EApp` desugar x `EApp` desugar y
desugar (SELeft x) = EVar (MkEVar "Left") `EApp` desugar x
desugar (SERight x) = EVar (MkEVar "Right") `EApp` desugar x
desugar (SEVar x) = EVar x
desugar (SEAnn x t) = EAnn (desugar x) t
desugar (SELam v x) = ELam v (desugar x)
desugar (SEApp x y) = desugar x `EApp` desugar y
