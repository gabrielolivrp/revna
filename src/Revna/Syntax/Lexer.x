{
{-# LANGUAGE RecordWildCards #-}

module Revna.Syntax.Lexer (runLexer, lexer, scan) where

import Data.ByteString qualified as BS
import Revna.Location
import Revna.Diagnostic
import Revna.Syntax.Monad
import Revna.Syntax.Token
}
$space   = [\t\v\f\ ]
$alpha = [a-zA-Z]
$newline = [\n\r]
$digit = 0-9

@integer = [$digit]+
@ident = $alpha [$alpha $digit \_]*

tokens :-

[\ \t]+                             ;
<0> "--" .* \n                      { \_ -> pushStartCode newline *> scan }
<0> \n                              { \_ -> pushStartCode newline *> scan }

<0> "module"                        { emitToken KwModule }
<0> "where"                         { layoutKw KwWhere }
<0> "let"                           { layoutKw KwLet }
<0> "in"                            { emitToken KwIn }
<0> "postulate"                     { emitToken KwPostulate }
<0> (∀|forall)                      { emitToken SymForall }
<0> (λ|\\)                          { emitToken SymLambda }
<0> "("                             { emitToken SymLParen }
<0> ")"                             { emitToken SymRParen }
<0> ":"                             { emitToken SymColon }
<0> "="                             { emitToken SymEq }
<0> "=>"                            { emitToken SymDoubleArrow }
<0> "->"                            { emitToken SymArrow }
<0> ","                             { emitToken SymComma  }
<0> @ident                          { emitIdent }

<layout> { "--" .* \n ; \n ; ()     { startLayout } }
<empty_layout> ()                   { emptyLayout }
<newline> { \n ; "--" .* \n ; ()    { offsideRule } }
<eof> ()                            { emitEOF }

{
layoutKw :: Token -> BS.ByteString -> ParserM (Loc Token)
layoutKw t _ = do
  pushStartCode layout
  emitToken' t

startLayout :: BS.ByteString -> ParserM (Loc Token)
startLayout _ = do
  popStartCode
  reference <- getLayout
  col <- getColumn
  if Just (LayoutColumn col) <= reference
    then pushStartCode empty_layout
    else pushLayout (LayoutColumn col)
  emitToken' TkVOpen

emptyLayout :: BS.ByteString -> ParserM (Loc Token)
emptyLayout _ = do
  popStartCode
  pushStartCode newline
  emitToken' TkVClose

offsideRule :: BS.ByteString -> ParserM (Loc Token)
offsideRule _ = do
  context <- getLayout
  col <- getColumn
  case context of
    Just (LayoutColumn col') ->
      case col `compare` col' of
        EQ -> do
          popStartCode
          emitToken' TkVSemi
        GT -> popStartCode *> scan
        LT -> do
          popLayout
          emitToken' TkVClose
    _ -> popStartCode *> scan

scan :: ParserM (Loc Token)
scan = do
  input <- getInput
  code <- getStartCode
  case alexScan input code of
    AlexEOF -> pushStartCode eof *> scan
    AlexError rest -> invalidLexemeError (lPos rest)
    AlexSkip rest _len -> setInput rest *> scan
    AlexToken rest len action -> do
      let token = BS.take len (lBuffer input)
      setInput rest
      action token

lexer :: (Loc Token -> ParserM a) -> ParserM a
lexer = (scan >>=)

runLexer :: FilePath -> BS.ByteString -> Either Diagnostic [Loc Token]
runLexer filepath text = runP filepath text [] go
  where
    go = do
      token <- scan
      case unlocated token of
        TkEOF -> pure []
        _ -> (token :) <$> go
}
