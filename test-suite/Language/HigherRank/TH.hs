{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveLift #-}

module Language.HigherRank.TH (exprQ, typeQ) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (Lift(..))

import Language.HigherRank.Parse
import Language.HigherRank.Types

deriving instance Lift EVar
deriving instance Lift Expr
deriving instance Lift TEVar
deriving instance Lift TVar
deriving instance Lift Type

voidQ :: QuasiQuoter
voidQ = QuasiQuoter
  { quoteExp = fail "cannot be used in expression position"
  , quotePat = fail "cannot be used in pattern position"
  , quoteType = fail "cannot be used in type position"
  , quoteDec = fail "cannot be used in declaration position"
  }

exprQ :: QuasiQuoter
exprQ = voidQ { quoteExp = either fail lift . parseExpr }

typeQ :: QuasiQuoter
typeQ = voidQ { quoteExp = either fail lift . parseType }
