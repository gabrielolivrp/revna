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

data TopLevel
  = FunDecl Name Term
  | FunTypeSig Name Term
  | PostulateDecl Name Term
  deriving (Show)

instance Show Term where
  show TmType = "Type"
  show TmRefl = "Refl"
  show (TmPos _ expr) = show expr
  show (TmVar (Name name)) = BS.unpack name
  show (TmLam (Name name) typ body) = "λ" <> BS.unpack name <> ": " <> show typ <> " => " <> show body
  show (TmId t1 t2) = show t1 <> "=" <> show t2
  show (TmForall (Name name) typ body) =
    if BS.unpack name == "_"
      then show typ <> " -> " <> show body
      else "∀(" <> BS.unpack name <> ": " <> show typ <> "), " <> show body
  show (TmApp func argm) = "(" <> show func <> ") (" <> show argm <> ")"
