module Revna.Diagnostic
  ( Diagnostic (..),
    Severity (..),
    Phase (..),
    Snippet (..),
    renderDiagnostic,
  )
where

import Data.ByteString qualified as Bs
import Revna.Location (Span)

data Diagnostic = Diagnostic
  { severity :: Severity,
    phase :: Phase,
    snippets :: [Snippet]
  }
  deriving (Show)

data Severity
  = Info
  | Warning
  | Error
  | Panic
  deriving (Show)

data Phase
  = Lexing
  | Parsing
  | Typing
  deriving (Show)

data Snippet = Snippet
  { pos :: Span,
    message :: String
  }
  deriving (Show)

renderDiagnostic :: Diagnostic -> Bs.ByteString -> String
renderDiagnostic d _ =
  unlines
    [ "gravidade: " <> show (severity d),
      "fase: " <> show (phase d),
      unlines $ map (\c -> "posição: " <> show (pos c) <> "\nmessage: " <> show (message c)) (snippets d)
    ]
