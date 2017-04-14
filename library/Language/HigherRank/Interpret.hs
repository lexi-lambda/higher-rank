module Language.HigherRank.Interpret (runInterpret) where

import qualified Data.Map as M

import Control.Monad.Except (MonadError, Except, runExcept, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, ask, local, runReaderT)

import Language.HigherRank.Typecheck

newtype Env = Env (M.Map EVar Expr)
  deriving (Eq, Show, Monoid)

newtype InterpretM a = InterpretM (ReaderT Env (Except String) a)
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadError String)

runInterpretM :: InterpretM a -> Either String a
runInterpretM (InterpretM x) = runExcept $ runReaderT x mempty

lookupVar :: EVar -> InterpretM Expr
lookupVar x = do
  Env env <- ask
  maybe (throwError $ "unbound variable " ++ show x) return $ M.lookup x env

withBinding :: EVar -> Expr -> InterpretM a -> InterpretM a
withBinding x e = local $ \(Env env) -> Env $ M.insert x e env

interpret :: Expr -> InterpretM Expr
interpret EUnit = return EUnit
interpret (EVar x) = lookupVar x
interpret (EAnn e _) = interpret e
interpret e@(ELam _ _) = return e
interpret (EApp f a) = interpret f >>= \case
  ELam x e -> interpret a >>= \b -> withBinding x b (interpret e)
  other -> throwError $ "cannot apply non-function value " ++ show other

runInterpret :: Expr -> Either String Expr
runInterpret = runInterpretM . interpret
