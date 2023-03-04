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
  = TmType -- "Type"
  | TmRefl -- "Refl"
  | TmVar Name -- "x"
  | TmPos Span Term -- term position
  | TmLam Name Term Term -- \x: T =\> M
  | TmApp Term Term -- m n
  | TmForall Name Term Term -- forall x: Type, x
  | TmId Term Term -- (plus 0 n) = n
  deriving (Show)

data TopLevel
  = FunDecl Name Term
  | FunTypeSig Name Term
  | PostulateDecl Name Term
  deriving (Show)
