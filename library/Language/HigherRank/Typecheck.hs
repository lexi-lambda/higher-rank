module Language.HigherRank.Typecheck
  ( Expr(..)
  , EVar(..)
  , Type(..)
  , TVar(..)
  , runInfer
  ) where

import qualified Data.Sequence as S

import Control.Monad (unless)
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError)
import Control.Monad.State (MonadState, State, evalState, get, gets, put, modify)
import Data.Foldable (toList)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Sequence (Seq)

newtype EVar = MkEVar { unEVar :: String }
  deriving (Eq, Ord, Show)

data Expr
  = EUnit
  | EVar EVar
  | EAnn Expr Type
  | ELam EVar Expr
  | EApp Expr Expr
  deriving (Eq, Ord, Show)

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

isMono :: Type -> Bool
isMono TUnit = True
isMono (TVar _) = True
isMono (TEVar _) = True
isMono (TArr a b) = isMono a && isMono b
isMono (TAll _ _) = False

data CtxMember
  = CtxVar TVar
  | CtxAssump EVar Type
  | CtxEVar TEVar
  | CtxSolved TEVar Type
  | CtxMarker TEVar
  deriving (Eq, Ord, Show)

newtype Ctx = Ctx (Seq CtxMember)
  deriving (Eq, Show, Monoid)

(|>) :: Ctx -> CtxMember -> Ctx
(Ctx ctx) |> mem = Ctx (ctx S.|> mem)

ctxElem :: CtxMember -> Ctx -> Bool
ctxElem x (Ctx ctx) = x `elem` ctx

ctxHole :: CtxMember -> Ctx -> Maybe (Ctx, Ctx)
ctxHole mem (Ctx ctx) = if mem `elem` ctx then Just (Ctx a, Ctx (S.drop 1 b)) else Nothing
  where (a, b) = S.breakl (== mem) ctx

ctxHole2 :: CtxMember -> CtxMember -> Ctx -> Maybe (Ctx, Ctx, Ctx)
ctxHole2 mem mem' ctx = do
  (a, ctx') <- ctxHole mem ctx
  (b, c) <- ctxHole mem' ctx'
  return (a, b, c)

ctxAssump :: Ctx -> EVar -> Maybe Type
ctxAssump (Ctx ctx) x = case assumptions of
    [CtxAssump _ t] -> Just t
    [] -> Nothing
    other -> error $ "ctxSolution: internal error — multiple types for variable: " ++ show other
  where isAssump (CtxAssump y _) = x == y
        isAssump _ = False
        assumptions = filter isAssump $ toList ctx

ctxSolution :: Ctx -> TEVar -> Maybe Type
ctxSolution (Ctx ctx) v = case solutions of
    [CtxSolved _ t] -> Just t
    [] -> Nothing
    other -> error $ "ctxSolution: internal error — multiple solutions for variable: " ++ show other
  where isSolution (CtxSolved u _) = v == u
        isSolution _ = False
        solutions = filter isSolution $ toList ctx

ctxUntil :: CtxMember -> Ctx -> Ctx
ctxUntil m (Ctx ctx) = Ctx $ S.takeWhileL (/= m) ctx

typeWF, (⊢) :: Ctx -> Type -> Either String ()
typeWF _ TUnit = return ()
typeWF ctx (TVar v) = unless (CtxVar v `ctxElem` ctx) $ Left $ "unbound type variable ‘" ++ unTVar v ++ "’"
typeWF ctx (TEVar v) = unless (CtxEVar v `ctxElem` ctx || hasSolution) $ Left $ "unbound existential variable ‘" ++ unTEVar v ++ "’"
  where hasSolution = isJust (ctxSolution ctx v)
typeWF ctx (TArr x y) = typeWF ctx x >> typeWF ctx y
typeWF ctx (TAll v t) = typeWF (ctx |> CtxVar v) t

(⊢) = typeWF

freeVars :: Type -> [TEVar]
freeVars TUnit = []
freeVars (TVar _) = []
freeVars (TEVar v) = [v]
freeVars (TArr a b) = freeVars a <> freeVars b
freeVars (TAll _ t) = freeVars t

applySubst :: Ctx -> Type -> Type
applySubst _ TUnit = TUnit
applySubst _ t@(TVar _) = t
applySubst ctx t@(TEVar v) = maybe t (applySubst ctx) (ctxSolution ctx v)
applySubst ctx (TArr a b) = TArr (applySubst ctx a) (applySubst ctx b)
applySubst ctx (TAll v t) = TAll v (applySubst ctx t)

inst :: (TVar, Type) -> Type -> Type
inst _ TUnit = TUnit
inst (v, s) t@(TVar v')
  | v == v' = s
  | otherwise = t
inst _ t@(TEVar _) = t
inst s (TArr a b) = TArr (inst s a) (inst s b)
inst s (TAll v t) = TAll v (inst s t)

--------------------------------------------------------------------------------

data CheckState = CheckState
  { checkCtx :: Ctx
  , checkNextEVar :: Integer
  } deriving (Eq, Show)

defCheckState :: CheckState
defCheckState = CheckState mempty 1

getCtx :: CheckM Ctx
getCtx = gets checkCtx

putCtx :: Ctx -> CheckM ()
putCtx ctx = get >>= \s -> put s { checkCtx = ctx }

modifyCtx :: (Ctx -> Ctx) -> CheckM ()
modifyCtx f = putCtx . f =<< getCtx

freshEVar :: CheckM TEVar
freshEVar = MkTEVar . ("a" ++) . show <$> gets checkNextEVar
         <* modify (\s -> s { checkNextEVar = checkNextEVar s + 1 })

checkTypeWF :: Type -> CheckM ()
checkTypeWF t = getCtx >>= \ctx -> either throwError return (typeWF ctx t)

newtype CheckM a = CheckM (ExceptT String (State CheckState) a)
  deriving (Functor, Applicative, Monad, MonadState CheckState, MonadError String)

runCheckM :: CheckM a -> Either String a
runCheckM (CheckM x) = evalState (runExceptT x) defCheckState

tySub :: Type -> Type -> CheckM ()
tySub TUnit TUnit = return ()
tySub (TVar a) (TVar b) | a == b = return ()
tySub (TEVar a) (TEVar b) | a == b = return ()
tySub (TArr a b) (TArr a' b') = tySub a' a >> tySub b b'
tySub (TAll v a) b = do
  â <- freshEVar
  let a' = inst (v, TEVar â) a
  modifyCtx (\c -> c |> CtxMarker â |> CtxEVar â)
  tySub a' b
  modifyCtx (ctxUntil (CtxMarker â))
tySub a (TAll v b) = do
  modifyCtx (|> CtxVar v)
  tySub a b
  modifyCtx (ctxUntil (CtxVar v))
tySub (TEVar â) a | â `notElem` freeVars a = instL â a
tySub a (TEVar â) | â `notElem` freeVars a = instR a â
tySub a b = throwError $ "type mismatch: expected " ++ show b ++ ", given " ++ show a

instL :: TEVar -> Type -> CheckM ()
instL â t = getCtx >>= go where
  -- Defer to a helper function so we can pattern match/guard against the
  -- current context.
  go ctx -- InstLSolve
    | True <- isMono t
    , Just (l, r) <- ctxHole (CtxEVar â) ctx
    , Right _ <- l ⊢ t
    = putCtx $ l |> CtxSolved â t <> r
  go ctx -- InstLReach
    | TEVar â' <- t
    , Just (l, m, r) <- ctxHole2 (CtxEVar â) (CtxEVar â') ctx
    = putCtx $ l |> CtxEVar â <> m |> CtxSolved â' (TEVar â) <> r
  go ctx -- InstLArr
    | Just (l, r) <- ctxHole (CtxEVar â) ctx
    , TArr a b <- t
    = do â1 <- freshEVar
         â2 <- freshEVar
         putCtx $ l |> CtxEVar â2 |> CtxEVar â1 |> CtxSolved â (TArr (TEVar â1) (TEVar â2)) <> r
         instR a â1
         ctx' <- getCtx
         instL â2 (applySubst ctx' b)
  go ctx -- InstLArrR
    | TAll b s <- t
    = do putCtx $ ctx |> CtxVar b
         instL â s
         Just (ctx', _) <- ctxHole (CtxVar b) <$> getCtx
         putCtx ctx'
  go _ = error $ "instL: failed to instantiate " ++ show â ++ " to " ++ show t

instR :: Type -> TEVar -> CheckM ()
instR t â = getCtx >>= go where
  -- Defer to a helper function so we can pattern match/guard against the
  -- current context.
  go ctx -- InstRSolve
    | True <- isMono t
    , Just (l, r) <- ctxHole (CtxEVar â) ctx
    , Right _ <- l ⊢ t
    = putCtx $ l |> CtxSolved â t <> r
  go ctx -- InstRReach
    | TEVar â' <- t
    , Just (l, m, r) <- ctxHole2 (CtxEVar â) (CtxEVar â') ctx
    = putCtx $ l |> CtxEVar â <> m |> CtxSolved â' (TEVar â) <> r
  go ctx -- InstRArr
    | Just (l, r) <- ctxHole (CtxEVar â) ctx
    , TArr a b <- t
    = do â1 <- freshEVar
         â2 <- freshEVar
         putCtx $ l |> CtxEVar â2 |> CtxEVar â1 |> CtxSolved â (TArr (TEVar â1) (TEVar â2)) <> r
         instL â1 a
         ctx' <- getCtx
         instR (applySubst ctx' b) â2
  go ctx -- InstRArrL
    | TAll b s <- t
    = do â' <- freshEVar
         putCtx $ ctx |> CtxMarker â' |> CtxEVar â'
         instR (inst (b, TEVar â') s) â
         Just (ctx', _) <- ctxHole (CtxMarker â') <$> getCtx
         putCtx ctx'
  go _ = error $ "instR: failed to instantiate " ++ show â ++ " to " ++ show t

check :: Expr -> Type -> CheckM ()
check EUnit TUnit = return ()
check e (TAll v a) = do
  modifyCtx (|> CtxVar v)
  check e a
  modifyCtx (ctxUntil (CtxVar v))
check (ELam x e) (TArr a b) = do
  modifyCtx (|> CtxAssump x a)
  check e b
  modifyCtx (ctxUntil (CtxAssump x a))
check e b = do
  a <- infer e
  ctx <- getCtx
  tySub (applySubst ctx a) (applySubst ctx b)

infer :: Expr -> CheckM Type
infer EUnit = return TUnit
infer (EVar x) = do
  ctx <- getCtx
  maybe (throwError $ "unbound variable " ++ show x) return (ctxAssump ctx x)
infer (EAnn e a) = checkTypeWF a >> check e a >> return a
infer (ELam x e) = do
  â <- freshEVar
  â' <- freshEVar
  modifyCtx (\c -> c |> CtxEVar â |> CtxEVar â' |> CtxAssump x (TEVar â))
  check e (TEVar â')
  modifyCtx (ctxUntil (CtxAssump x (TEVar â)))
  return $ TArr (TEVar â) (TEVar â')
infer (EApp e1 e2) = do
  a <- infer e1
  ctx <- getCtx
  inferApp (applySubst ctx a) e2

inferApp :: Type -> Expr -> CheckM Type
inferApp (TAll v a) e = do
  â <- freshEVar
  modifyCtx (|> CtxEVar â)
  inferApp (inst (v, TEVar â) a) e
inferApp (TEVar â) e = do
  â1 <- freshEVar
  â2 <- freshEVar
  modifyCtx (\c -> c |> CtxEVar â2 |> CtxEVar â1 |> CtxSolved â (TArr (TEVar â1) (TEVar â2)))
  check e (TEVar â1)
  return $ TEVar â2
inferApp (TArr a c) e = check e a >> return c
inferApp t e = throwError $ "cannot apply expression of type " ++ show t ++ " to expression " ++ show e

runInfer :: Expr -> Either String Type
runInfer e = do
  (t, ctx) <- runCheckM ((,) <$> infer e <*> getCtx)
  return $ applySubst ctx t
