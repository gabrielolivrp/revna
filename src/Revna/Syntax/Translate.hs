module Revna.Syntax.Translate
  ( translModule,
    translTopLevel,
    translTree,
  )
where

import Data.List.NonEmpty qualified as NE
import Revna.Core.Term qualified as Tm
import Revna.Location (HasSpan (getSpan), Span)
import Revna.Syntax.Tree qualified as Tr

translModule :: Tr.Module -> Tm.Module
translModule (Tr.Module name topLevels) =
  Tm.Module (translName name) (map translTopLevel topLevels)

translTopLevel :: Tr.TopLevel -> Tm.TopLevel
translTopLevel (Tr.FunTypeDecl sp name expr) =
  Tm.FunTypeSig sp (translName name) (translTree expr)
translTopLevel (Tr.FunDecl sp name expr) =
  Tm.FunDecl sp (translName name) (translTree expr)
translTopLevel (Tr.PostulateDecl sp name expr) =
  Tm.PostulateDecl sp (translName name) (translTree expr)

translTree :: Tr.Tree -> Tm.Term
translTree (Tr.Var sp var) =
  case var of
    Tr.Name "Type" -> Tm.TmType sp
    Tr.Name "Refl" -> Tm.TmRefl sp
    _ -> Tm.TmVar sp (translName var)
translTree (Tr.Lam sp (b NE.:| bs) body) =
  let body' = translTree body
   in translLam body' (b : bs) (Tm.TmLam sp)
translTree (Tr.App sp funct (a NE.:| as)) =
  let lastArgumSpan = getSpan (last (a : as))
      funct' = Tm.TmApp (sp <> lastArgumSpan) (translTree funct) (translTree a)
   in foldl (\f a' -> Tm.TmApp (getSpan f <> getSpan a') f (translTree a')) funct' as
translTree (Tr.Forall sp (b NE.:| bs) body) =
  let lastBindSpan = getSpan (last (b : bs))
      body' = translTree body
   in translLam body' (b : bs) (Tm.TmForall (sp <> lastBindSpan))
translTree (Tr.Arrow sp param body) =
  let param' = translTree param
      body' = translTree body
   in Tm.TmForall sp (Tm.Name "_") param' body'
translTree (Tr.Let _sp (b NE.:| bs) body) =
  let body' = translTree body
   in translLet body' (b : bs)
translTree (Tr.Eq sp t1 t2) =
  Tm.TmEq sp (translTree t1) (translTree t2)

translLet ::
  Tm.Term ->
  [(Tr.Name, Tr.Tree, Tr.Tree)] ->
  Tm.Term
translLet body [] = body
translLet body ((param, typ, expr) : xs) =
  let body' = translLet body xs
      argum = translTree expr
      funct = Tm.TmLam (getSpan body' <> getSpan body) (translName param) (translTree typ) body'
   in Tm.TmApp (getSpan funct <> getSpan argum) funct argum

translLam ::
  Tm.Term ->
  [Tr.Bind] ->
  (Tm.Name -> Tm.Term -> Tm.Term -> Tm.Term) ->
  Tm.Term
translLam body [] _ = body
translLam body (bind : xs) f =
  let (_, name', typ') = translBind bind
      body' = translLam body xs f
   in f name' typ' body'

translBind :: Tr.Bind -> (Span, Tm.Name, Tm.Term)
translBind (Tr.Bind sp name typ) = (sp, translName name, translTree typ)

translName :: Tr.Name -> Tm.Name
translName (Tr.Name name) = Tm.Name name
