module Language.HigherRank.Interpret (runInterpret) where

import qualified Data.Map as M

import Control.Monad.Except (MonadError, Except, runExcept, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, ask, local, runReaderT)

import Language.HigherRank.Print (printReducedExpr)
import Language.HigherRank.Types

newtype InterpretM a = InterpretM (ReaderT Env (Except String) a)
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadError String)

runInterpretM :: InterpretM a -> Either String a
runInterpretM (InterpretM x) = runExcept $ runReaderT x mempty

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

interpret :: Expr -> InterpretM ReducedExpr
interpret EUnit = return REUnit
interpret (EVar x) = lookupVar x
interpret (EAnn e _) = interpret e
interpret (ELam x e) = RELam <$> close <*> pure x <*> pure e
interpret (EApp f a) = interpret f >>= \case
  RELam env x e -> interpret a >>= \b -> open env (withBinding x b (interpret e))
  other -> throwError $ "cannot apply non-function value " ++ printReducedExpr other

runInterpret :: Expr -> Either String ReducedExpr
runInterpret = runInterpretM . interpret
