module Revna.Typer.Infer
  ( infer,
    check,
    checkModule,
  )
where

import Control.Monad
import Data.Foldable (foldlM)
import Revna.Context qualified as Ctx
import Revna.Core.Normal
import Revna.Core.Term
import Revna.Diagnostic
import Revna.Typer.Env

checkModule :: Typer m => Module -> m ()
checkModule (Module _ decls) = do
  _ <- foldlM (\ctx toplevel -> runLocalCtx (const ctx) (checkDecl toplevel)) Ctx.emptyContext decls
  pure ()

checkDecl :: Typer m => TopLevel -> m Ctx.Context
checkDecl (PostulateDecl name typ) = do
  ctx <- getContext
  _ <- checkType typ
  pure (Ctx.assumeType ctx name typ)
checkDecl (FunTypeSig name typ) = do
  ctx <- getContext
  _ <- checkType (Ctx.subst (Ctx.bindings ctx) typ)
  pure (Ctx.assumeType ctx name typ)
checkDecl (FunDecl name expr) = do
  ctx <- getContext
  let typeSig = Ctx.lookupType ctx name
   in checkFun name expr typeSig

checkFun :: Typer m => Name -> Term -> Maybe Term -> m Ctx.Context
checkFun name expr (Just typeSig) = do
  ctx <- getContext
  _ <- checkExpr expr typeSig
  pure (Ctx.assumeBind ctx name expr)
checkFun name expr Nothing = do
  ctx <- getContext
  typT <- infer (Ctx.subst (Ctx.bindings ctx) expr)
  let ctx' = Ctx.assumeBind ctx name expr
  pure (Ctx.assumeType ctx' name typT)

check :: Typer m => Term -> Term -> m Term
check (TmPos sp expr) ty = extendPos sp (check expr ty)
check (TmLam param typ body) ty@(TmForall _ typ' body')
  | True <- alphaEq typ typ' = do
      _ <- checkType typ'
      runLocalCtx (\ctx -> Ctx.assumeType ctx param typ') (checkExpr body body')
      pure ty
  | otherwise = emitDiagnostic Error ["type mismatch"]
check (TmLam {}) nf = emitDiagnostic Error ["not a function type: " <> show nf]
check TmRefl ty@(TmId a b) = do
  if alphaEq a b
    then pure ty
    else emitDiagnostic Error ["refl"]
check TmRefl _ = emitDiagnostic Error ["refl"]
check tm ty = do
  ty' <- infer tm
  if alphaEq ty' ty
    then pure ty'
    else emitDiagnostic Error ["check"]

infer :: Typer m => Term -> m Term
infer TmType = pure TmType
infer (TmPos sp expr) = extendPos sp (infer expr)
infer (TmVar name) = do
  ctx <- getContext
  case Ctx.lookupType ctx name of
    Just expr ->
      case expr of
        TmPos _ e -> pure e
        _ -> pure expr
    Nothing -> emitDiagnostic Error ["Unbounded variable"]
infer (TmLam param typ body) = do
  _ <- checkType typ
  runLocalCtx (\ctx -> Ctx.assumeType ctx param typ) $ do
    bodyT <- infer body
    let lamT = TmForall param typ bodyT
    _ <- checkType lamT
    pure lamT
infer (TmForall param typ body) = do
  _ <- checkType typ
  runLocalCtx (\ctx' -> Ctx.assumeType ctx' param typ) $ do
    _ <- checkType body
    pure TmType
infer (TmApp funct argum) = do
  functT <- norm <$> infer funct
  (param, typ, body) <-
    case functT of
      TmForall param typ body -> pure (param, typ, body)
      x -> emitDiagnostic Error ["Not a function " <> show x]
  argumT <- norm <$> infer argum
  unless
    (betaEq typ argumT)
    ( emitDiagnostic Error ["Type mismatch"]
    )
  pure (subst body param argum)
infer (TmId expr1 expr2) = do
  expr1' <- infer expr1
  _ <- checkExpr expr2 expr1'
  pure TmType
infer _ = emitDiagnostic Error ["infer"]

checkExpr :: Typer m => Term -> Term -> m ()
checkExpr expr typ = do
  ctx <- getContext
  let typ' = norm (Ctx.subst (Ctx.bindings ctx) typ)
  let expr' = norm (Ctx.subst (Ctx.bindings ctx) expr)
  _ <- check expr' typ'
  pure ()

checkType :: Typer m => Term -> m ()
checkType typ = do
  k <- norm <$> infer typ
  case k of
    TmType -> pure ()
    _ -> emitDiagnostic Error ["expected a kind"]
