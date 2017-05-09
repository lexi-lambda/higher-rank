module Language.HigherRank.Interpret (runInterpret) where

import qualified Data.Map as M

import Control.Monad.Except (runExcept, throwError)
import Control.Monad.Reader (ask, local, runReaderT)

import Language.HigherRank.Print (printReducedExpr)
import Language.HigherRank.Types

baseEnv :: Env
baseEnv = Env $ M.fromList
  [ (MkEVar "Tuple", mkTuple)
  , (MkEVar "Left", mkLeft)
  , (MkEVar "Right", mkRight)
  , (MkEVar "tuple", unTuple)
  , (MkEVar "either", unEither)
  ] where
    mkTuple = REPrim . PrimOp "Tuple" $ \x ->
      return . REPrim . PrimOp "Tuple" $ \y ->
        return $ RETuple x y
    mkLeft = REPrim . PrimOp "Left" $ \x ->
      return $ RELeft x
    mkRight = REPrim . PrimOp "Right" $ \x ->
      return $ RERight x
    unTuple = REPrim . PrimOp "tuple" $ \f ->
      return . REPrim . PrimOp "tuple" $ \case
        RETuple x y -> f `apply` x >>= (`apply` y)
        other -> throwError $ "tuple: expected a tuple, given " ++ printReducedExpr other
    unEither = REPrim . PrimOp "either" $ \f ->
      return . REPrim . PrimOp "either" $ \g ->
        return . REPrim . PrimOp "either" $ \case
          RELeft x -> f `apply` x
          RERight x -> g `apply` x
          other -> throwError $ "either: expected left or right, given " ++ printReducedExpr other

runInterpretM :: InterpretM a -> Either String a
runInterpretM (InterpretM x) = runExcept $ runReaderT x baseEnv

lookupVar :: EVar -> InterpretM ReducedExpr
lookupVar x = do
  Env env <- ask
  maybe (throwError $ "unbound variable " ++ unEVar x) return $ M.lookup x env

withBinding :: EVar -> ReducedExpr -> InterpretM a -> InterpretM a
withBinding x e = local $ \(Env env) -> Env $ M.insert x e env

close :: InterpretM Env
close = ask

open :: Env -> InterpretM a -> InterpretM a
open env = local (const env)

apply :: ReducedExpr -> ReducedExpr -> InterpretM ReducedExpr
apply f a = case f of
  RELam env x e -> open env (withBinding x a (interpret e))
  REPrim op -> primOpFn op a
  other -> throwError $ "cannot apply non-function value " ++ printReducedExpr other

interpret :: Expr -> InterpretM ReducedExpr
interpret EUnit = return REUnit
interpret (EVar x) = lookupVar x
interpret (EAnn e _) = interpret e
interpret (ELam x e) = RELam <$> close <*> pure x <*> pure e
interpret (EApp f x) = do
  f' <- interpret f
  x' <- interpret x
  apply f' x'

runInterpret :: Expr -> Either String ReducedExpr
runInterpret = runInterpretM . interpret
