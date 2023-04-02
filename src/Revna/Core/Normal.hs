module Revna.Core.Normal
  ( subst,
    fv,
    alphaEq,
    betaEq,
    beta,
    norm,
  )
where

import Data.Set qualified as S
import Revna.Core.Term (Name (..), Term (..))

fresh :: Name -> S.Set Name -> Name
fresh x@(Name name) xs
  | x `S.member` xs =
      let x' = Name (name <> "'")
       in fresh x' xs
  | otherwise = x

fv :: Term -> S.Set Name
fv TmType = S.empty
fv TmRefl = S.empty
fv (TmPos _ expr) = fv expr
fv (TmVar name) = S.singleton name
fv (TmId expr1 expr2) = S.union (fv expr1) (fv expr2)
fv (TmApp funct argum) = S.union (fv funct) (fv argum)
fv (TmLam param typ body) = fv typ `S.union` S.delete param (fv body)
fv (TmForall param typ body) = fv typ `S.union` S.delete param (fv body)

subst :: Term -> Name -> Term -> Term
subst TmType _ _ = TmType
subst TmRefl _ _ = TmRefl
subst var@(TmVar v) x t
  | x == v = t
  | otherwise = var
subst (TmLam param typ body) x t
  | param == x = TmLam param (subst typ x t) body
  | param /= x && param `S.notMember` fv t =
      TmLam param (subst typ x t) (subst body x t)
  | otherwise =
      let z = fresh param (fv body `S.union` fv t)
       in subst (TmLam z typ (subst body param (TmVar z))) x t
subst (TmForall param typ body) x t
  | param == x = TmForall param (subst typ x t) body
  | param /= x && param `S.notMember` fv t =
      TmForall param (subst typ x t) (subst body x t)
  | otherwise =
      let z = fresh param (fv body `S.union` fv t)
       in subst (TmForall z typ (subst body param (TmVar z))) x t
subst (TmApp funct argum) x t = TmApp (subst funct x t) (subst argum x t)
subst (TmId expr1 expr2) x t = TmId (subst expr1 x t) (subst expr2 x t)
subst (TmPos sp expr) x t = TmPos sp (subst expr x t)

alphaEq :: Term -> Term -> Bool
alphaEq TmType TmType = True
alphaEq TmRefl TmRefl = True
alphaEq (TmVar name) (TmVar name') = name == name'
alphaEq (TmForall param typ body) (TmForall param' typ' body')
  | alphaEq typ typ' =
      let temp = fresh param (fv body `S.union` fv body')
       in alphaEq (rename param temp body) (rename param' temp body')
  | otherwise = False
alphaEq (TmLam param typ body) (TmLam param' typ' body')
  | alphaEq typ typ' =
      let temp = fresh param (fv body `S.union` fv body')
       in alphaEq (rename param temp body) (rename param' temp body')
  | otherwise = False
alphaEq (TmApp funct argum) (TmApp funct' argum') =
  alphaEq funct funct' && alphaEq argum argum'
alphaEq (TmId expr1 expr2) (TmId expr1' expr2') =
  alphaEq expr1 expr1' && alphaEq expr2 expr2'
alphaEq (TmPos _ expr) (TmPos _ expr') = alphaEq expr expr'
alphaEq expr (TmPos _ expr') = alphaEq expr expr'
alphaEq (TmPos _ expr) expr' = alphaEq expr expr'
alphaEq _ _ = False

rename :: Name -> Name -> Term -> Term
rename _ _ TmType = TmType
rename _ _ TmRefl = TmRefl
rename name newName var@(TmVar name')
  | name' == name = TmVar newName
  | otherwise = var
rename name newName (TmLam param typ body)
  | param == name =
      let typ' = rename name newName typ
       in TmLam param typ' body
  | otherwise =
      let typ' = rename name newName typ
          body' = rename name newName body
       in TmLam param typ' body'
rename name newName (TmForall param typ body)
  | param == name =
      let typ' = rename name newName typ
       in TmForall param typ' body
  | otherwise =
      let typ' = rename name newName typ
          body' = rename name newName body
       in TmForall param typ' body'
rename name newName (TmApp funct argum) =
  let funct' = rename name newName funct
      argum' = rename name newName argum
   in TmApp funct' argum'
rename name newName (TmId expr1 expr2) =
  let expr1' = rename name newName expr1
      expr2' = rename name newName expr2
   in TmId expr1' expr2'
rename name newName (TmPos sp expr) =
  TmPos sp (rename name newName expr)

betaEq :: Term -> Term -> Bool
betaEq expr expr' = alphaEq (norm expr) (norm expr')

beta :: Term -> Term
beta (TmPos _ expr) = beta expr
beta (TmId funct argum) =
  TmId (beta funct) (beta argum)
beta (TmLam param typ body) =
  TmLam param (beta typ) (beta body)
beta (TmApp (TmLam param _ body) argum) =
  subst body param (beta argum)
beta (TmApp funct argum) =
  TmApp (beta funct) (beta argum)
beta (TmForall param typ body) =
  TmForall param (beta typ) (beta body)
beta expr = expr

-- | Normal form
norm :: Term -> Term
norm expr =
  let expr' = beta expr
   in if alphaEq expr' expr
        then expr'
        else norm expr'
