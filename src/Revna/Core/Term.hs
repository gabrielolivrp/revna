module Revna.Core.Term
  ( Name (..),
    TopLevel (..),
    Module (..),
    Term (..),
  )
where

import Data.ByteString.Char8 qualified as BS
import Revna.Location

newtype Name = Name BS.ByteString
  deriving (Eq, Show, Ord)

data Module = Module Name [TopLevel]
  deriving (Show)

data Term
  = TmType Span -- "Type"
  | TmRefl Span -- "Refl"
  | TmVar Span Name -- "x"
  | TmLam Span Name Term Term -- \x: T =\> M
  | TmApp Span Term Term -- m n
  | TmForall Span Name Term Term -- forall x: Type, x
  | TmEq Span Term Term -- (plus 0 n) = n
  deriving (Show)

data TopLevel
  = FunDecl Span Name Term
  | FunTypeSig Span Name Term
  | PostulateDecl Span Name Term
  deriving (Show)

instance HasSpan TopLevel where
  getSpan = \case
    FunDecl sp _ _ -> sp
    FunTypeSig sp _ _ -> sp
    PostulateDecl sp _ _ -> sp

instance HasSpan Term where
  getSpan = \case
    TmType sp -> sp
    TmRefl sp -> sp
    TmVar sp _ -> sp
    TmLam sp _ _ _ -> sp
    TmApp sp _ _ -> sp
    TmForall sp _ _ _ -> sp
    TmEq sp _ _ -> sp
