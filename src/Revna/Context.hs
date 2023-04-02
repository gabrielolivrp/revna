module Revna.Context
  ( Context (..),
    emptyContext,
    assumeBind,
    assumeType,
    lookupType,
    lookupBind,
    subst,
  )
where

import Data.Map qualified as M
import Revna.Core.Term

-- TODO: make instance for show context
data Context = Context
  { typings :: M.Map Name Term,
    bindings :: M.Map Name Term
  }

emptyContext :: Context
emptyContext = Context M.empty M.empty

assumeBind :: Context -> Name -> Term -> Context
assumeBind ctx name typ =
  ctx {bindings = M.insert name typ (bindings ctx)}

lookupBind :: Context -> Name -> Maybe Term
lookupBind ctx name = M.lookup name (bindings ctx)

assumeType :: Context -> Name -> Term -> Context
assumeType ctx name value =
  ctx {typings = M.insert name value (typings ctx)}

lookupType :: Context -> Name -> Maybe Term
lookupType ctx name = M.lookup name (typings ctx)

subst :: M.Map Name Term -> Term -> Term
subst ctx var@(TmVar name) =
  case M.lookup name ctx of
    Just expr -> subst ctx expr
    Nothing -> var
subst ctx (TmPos sp expr) = TmPos sp (subst ctx expr)
subst ctx (TmLam param typ body) =
  TmLam param (subst ctx typ) (subst (M.delete param ctx) body)
subst ctx (TmForall param typ body) =
  TmForall param (subst ctx typ) (subst (M.delete param ctx) body)
subst ctx (TmApp funct argum) =
  TmApp (subst ctx funct) (subst ctx argum)
subst ctx (TmId funct argum) =
  TmId (subst ctx funct) (subst ctx argum)
subst _ expr = expr

instance Show Context where
  show ctx = "\n[typings]: \n" <> go (typings ctx) <> "\n[bidings]: \n" <> go (bindings ctx)
    where
      go c = foldl (\a b -> a <> build b <> "\n") "" $ M.toList c
      build (Name key, val) = show key <> ": " <> show val
