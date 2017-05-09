module Language.HigherRank.Types where

import Control.Monad.Except (MonadError, Except)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Map (Map)

--------------------------------------------------------------------------------
-- Sugared expressions (parser & desugarer)

data SugaredExpr
  = SEUnit
  | SETuple SugaredExpr SugaredExpr
  | SELeft SugaredExpr
  | SERight SugaredExpr
  | SEVar EVar
  | SEAnn SugaredExpr Type
  | SELam EVar SugaredExpr
  | SEApp SugaredExpr SugaredExpr
  deriving (Eq, Ord, Show)

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
  | TProduct Type Type
  | TSum Type Type
  | TVar TVar
  | TEVar TEVar
  | TArr Type Type
  | TAll TVar Type
  deriving (Eq, Ord, Show)

infixr 9 `TArr`

--------------------------------------------------------------------------------
-- Reduced expressions (interpreter)

data PrimOp = PrimOp
  { primOpName :: String
  , primOpFn :: ReducedExpr -> InterpretM ReducedExpr }

instance Show PrimOp where
  showsPrec n (PrimOp name _) = showParen (n > 10) . showString $
    "PrimOp " ++ show name ++ " <native code>"

data ReducedExpr
  = REUnit
  | RETuple ReducedExpr ReducedExpr
  | RELeft ReducedExpr
  | RERight ReducedExpr
  | RELam Env EVar Expr
  | REPrim PrimOp
  deriving (Show)

newtype Env = Env (Map EVar ReducedExpr)
  deriving (Show, Monoid)

newtype InterpretM a = InterpretM (ReaderT Env (Except String) a)
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadError String)
