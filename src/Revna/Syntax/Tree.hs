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

newtype Name = Name BS.ByteString
  deriving (Show, Eq)

data Bind = Bind Name Tree
  deriving (Show, Eq)

data Tree
  = -- | variables `x`
    Var Name
  | -- | lambda abstractions `λ(n_1: Type_1) (n_2: Type_2)...(n_x: Type_x) =\> M`
    Lam (NE.NonEmpty Bind) Tree
  | -- | lambda applications `M n_1 n_2...n_x`
    App Tree (NE.NonEmpty Tree)
  | -- | function types `∀(a_1: Type_1) (a_2: Type_2)...(a_n: Type_n), a -> b`
    Forall (NE.NonEmpty Bind) Tree
  | -- | let expression `let x: type = M in N`
    Let (NE.NonEmpty (Name, Tree, Tree)) Tree
  | -- | arrow type `a -> b`
    Arrow Tree Tree
  | -- | Expr = Expr
    Eq Tree Tree
  deriving (Show, Eq)

data TopLevel
  = -- | id : ∀(a: Type), a -> a
    FunTypeDecl Name Tree
  | -- | id = λa: Type =\> λx: a =\> a
    FunDecl Name Tree
  | -- | postulate World : Type
    PostulateDecl Name Tree
  deriving (Show, Eq)

data Module = Module Name [TopLevel]
  deriving (Show, Eq)
