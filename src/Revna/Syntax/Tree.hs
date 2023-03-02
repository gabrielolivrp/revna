module Revna.Syntax.Tree
  ( Tree (..),
    Name (..),
    Bind (..),
    TopLevel (..),
    Module (..),
  )
where

import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Revna.Location

newtype Name = Name BS.ByteString
  deriving (Show, Eq)

data Bind = Bind Span Name Tree
  deriving (Show, Eq)

data Tree
  = -- | variables `x`
    Var Span Name
  | -- | lambda abstractions `λ(n_1: Type_1) (n_2: Type_2)...(n_x: Type_x) =\> M`
    Lam Span (NE.NonEmpty Bind) Tree
  | -- | lambda applications `M n_1 n_2...n_x`
    App Span Tree (NE.NonEmpty Tree)
  | -- | function types `∀(a_1: Type_1) (a_2: Type_2)...(a_n: Type_n), a -> b`
    Forall Span (NE.NonEmpty Bind) Tree
  | -- | let expression `let x: type = M in N`
    Let Span (NE.NonEmpty (Name, Tree, Tree)) Tree
  | -- | arrow type `a -> b`
    Arrow Span Tree Tree
  | -- | Expr = Expr
    Eq Span Tree Tree
  deriving (Show, Eq)

data TopLevel
  = -- | id : ∀(a: Type), a -> a
    FunTypeDecl Span Name Tree
  | -- | id = λa: Type =\> λx: a =\> a
    FunDecl Span Name Tree
  | -- | postulate World : Type
    PostulateDecl Span Name Tree
  deriving (Show, Eq)

data Module = Module Name [TopLevel]
  deriving (Show, Eq)

instance HasSpan Tree where
  getSpan = \case
    Var sp _ -> sp
    Lam sp _ _ -> sp
    App sp _ _ -> sp
    Forall sp _ _ -> sp
    Let sp _ _ -> sp
    Arrow sp _ _ -> sp
    Eq sp _ _ -> sp

instance HasSpan TopLevel where
  getSpan = \case
    FunTypeDecl sp _ _ -> sp
    FunDecl sp _ _ -> sp
    PostulateDecl sp _ _ -> sp

instance HasSpan Bind where
  getSpan (Bind sp _ _) = sp
