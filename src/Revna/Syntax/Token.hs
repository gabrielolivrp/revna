module Revna.Syntax.Token (Token (..)) where

import Data.ByteString qualified as BS

data Token
  = -- | the "module" keyword
    KwModule
  | -- | the "where" keyword
    KwWhere
  | -- | the "data" keyword
    KwData
  | -- | the "let" keyword
    KwLet
  | -- | the "in" keyword
    KwIn
  | -- | the "postulate" keyword
    KwPostulate
  | -- | the "∀" symbol
    SymForall
  | -- | the "λ" symbol
    SymLambda
  | -- | the "->" symbol
    SymArrow
  | -- | the "\=>" symbol
    SymDoubleArrow
  | -- | the ":" symbol
    SymColon
  | -- | the "=" symbol
    SymEq
  | -- | the "_" symbol
    SymWild
  | -- | the "," symbol
    SymComma
  | -- | the "(" symbol
    SymLParen
  | -- | the ")" symbol
    SymRParen
  | -- | the identifier
    TkIdent BS.ByteString
  | -- | the virtual open block
    TkVOpen
  | -- | the virtual separate block
    TkVSemi
  | -- | the virtual close block
    TkVClose
  | -- | the EOF token
    TkEOF
  deriving (Show, Eq)
