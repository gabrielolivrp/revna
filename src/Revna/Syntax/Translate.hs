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
translTopLevel (Tr.FunTypeDecl _ name expr) =
  Tm.FunTypeSig (translName name) (translTree expr)
translTopLevel (Tr.FunDecl _ name expr) =
  Tm.FunDecl (translName name) (translTree expr)
translTopLevel (Tr.PostulateDecl _ name expr) =
  Tm.PostulateDecl (translName name) (translTree expr)

translTree :: Tr.Tree -> Tm.Term
translTree (Tr.Var sp name) =
  case name of
    Tr.Name "Type" -> Tm.TmPos sp Tm.TmType
    Tr.Name "Refl" -> Tm.TmPos sp Tm.TmRefl
    _ ->
      let name' = translName name
       in Tm.TmPos sp (Tm.TmVar name')
translTree (Tr.Lam sp (b NE.:| bs) body) =
  let body' = translTree body
   in Tm.TmPos sp $ translLam body' (b : bs) Tm.TmLam
translTree (Tr.App sp funct (a NE.:| as)) =
  let sp' = sp <> getSpan (last (a : as))
      funct' = Tm.TmPos sp' $ Tm.TmApp (translTree funct) (translTree a)
   in foldl (\f@(Tm.TmPos ps _) a' -> Tm.TmPos (ps <> getSpan a') $ Tm.TmApp f (translTree a')) funct' as
translTree (Tr.Forall sp (b NE.:| bs) body) =
  let sp' = sp <> getSpan (last (b : bs))
      body' = translTree body
   in Tm.TmPos sp' $ translLam body' (b : bs) Tm.TmForall
translTree (Tr.Arrow sp param body) =
  let param' = translTree param
      body' = translTree body
   in Tm.TmPos sp $ Tm.TmForall (Tm.Name "_") param' body'
translTree (Tr.Let _sp (b NE.:| bs) body) =
  translLet body (b : bs)
translTree (Tr.Id sp expr1 expr2) =
  Tm.TmPos sp $ Tm.TmId (translTree expr1) (translTree expr2)

translLet ::
  Tr.Tree ->
  [(Span, Tr.Name, Tr.Tree, Tr.Tree)] ->
  Tm.Term
translLet body [] = translTree body
translLet body ((sp, param, typ, expr) : xs) =
  let body'@(Tm.TmPos sp' _) = translLet body xs
      argum@(Tm.TmPos sp'' _) = translTree expr
      funct = Tm.TmPos (sp <> sp') $ Tm.TmLam (translName param) (translTree typ) body'
   in Tm.TmPos ((sp <> sp') <> sp'') $ Tm.TmApp funct argum

translLam ::
  Tm.Term ->
  [Tr.Bind] ->
  (Tm.Name -> Tm.Term -> Tm.Term -> Tm.Term) ->
  Tm.Term
translLam body [] _ = body
translLam body (bind : xs) f =
  let (sp, name', typ') = translBind bind
      body'@(Tm.TmPos sp' _) = translLam body xs f
   in Tm.TmPos (sp <> sp') $ f name' typ' body'

translBind :: Tr.Bind -> (Span, Tm.Name, Tm.Term)
translBind (Tr.Bind sp name typ) = (sp, translName name, translTree typ)

translName :: Tr.Name -> Tm.Name
translName (Tr.Name name) = Tm.Name name
