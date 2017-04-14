module Language.HigherRank.Types where

import Data.Map (Map)

--------------------------------------------------------------------------------
-- Expressions (interpreter & typechecker)

newtype EVar = MkEVar { unEVar :: String }
  deriving (Eq, Ord, Show)

data Expr
  = EUnit
  | EVar EVar
  | EAnn Expr Type
  | ELam EVar Expr
  | EApp Expr Expr
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Types (typechecker)

newtype TVar = MkTVar { unTVar :: String }
  deriving (Eq, Ord, Show)

newtype TEVar = MkTEVar { unTEVar :: String }
  deriving (Eq, Ord, Show)

data Type
  = TUnit
  | TVar TVar
  | TEVar TEVar
  | TArr Type Type
  | TAll TVar Type
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Reduced expressions (interpreter)

data ReducedExpr
  = REUnit
  | RELam Env EVar Expr
  deriving (Eq, Show)

newtype Env = Env (Map EVar ReducedExpr)
  deriving (Eq, Show, Monoid)
