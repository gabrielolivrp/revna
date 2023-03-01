module Revna.Diagnostic
  ( Diagnostic (..),
    Severity (..),
    Phase (..),
    Snippet (..),
  )
where

import Revna.Location (Span)

data Diagnostic = Diagnostic Severity Phase [Snippet]
  deriving (Show)

data Severity
  = Info
  | Warning
  | Error
  | Panic
  deriving (Show)

data Phase
  = Lexer
  | Parser
  | Typing
  deriving (Show)

data Snippet = Snippet Span String
  deriving (Show)
