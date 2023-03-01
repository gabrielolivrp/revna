{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Revna.Syntax.Monad
  ( AlexInput (..),
    ParseState (..),
    ParserM (..),
    Layout (..),
    alexGetByte,
    initParse,
    runP,
    location,
    position,
    located,
    emitToken,
    emitIdent,
    emitToken',
    emitEOF,
    getColumn,
    getInput,
    setInput,
    getLayout,
    pushLayout,
    popLayout,
    getStartCode,
    pushStartCode,
    popStartCode,
    invalidLexemeError,
  )
where

import Control.Monad.Except (Except, MonadError (throwError), runExcept)
import Control.Monad.State
  ( MonadState,
    StateT (StateT),
    evalStateT,
    gets,
    modify,
  )
import Data.ByteString qualified as BS
import Data.ByteString.Internal (w2c)
import Data.List (uncons)
import Data.List.NonEmpty qualified as NE
import Data.Word (Word8)
import Revna.Diagnostic
  ( Diagnostic (..),
    Phase (Lexer),
    Severity (Error),
    Snippet (..),
  )
import Revna.Location
  ( Loc (..),
    Position (posCol),
    Span (Span),
    movePos,
    startPos,
  )
import Revna.Syntax.Token (Token (TkEOF, TkIdent, TkVClose))

-- | Lexer Functions
type Byte = Word8

data AlexInput = AlexInput
  { -- | Current position
    lPos :: Position,
    -- | Previously read character
    lPrevChar :: Char,
    -- | Current text
    lBuffer :: BS.ByteString
  }
  deriving (Show)

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte (AlexInput pos _ buffer) = do
  (ch, buffer') <- BS.uncons buffer
  let w' = w2c ch
  pure
    ( ch,
      AlexInput
        { lPos = movePos pos w',
          lPrevChar = w',
          lBuffer = buffer'
        }
    )

emitToken :: Token -> BS.ByteString -> ParserM (Loc Token)
emitToken token _ = located token

emitToken' :: Token -> ParserM (Loc Token)
emitToken' = located

emitIdent :: BS.ByteString -> ParserM (Loc Token)
emitIdent = located . TkIdent

emitEOF :: BS.ByteString -> ParserM (Loc Token)
emitEOF _ =
  getLayout >>= \case
    Just _ -> do
      popLayout
      located TkVClose
    Nothing -> do
      popStartCode
      located TkEOF

-- | Parser functions
data ParseState = ParseState
  { -- | File path
    pFilepath :: FilePath,
    -- | Current text of the source file
    pInput :: AlexInput,
    -- | Start codes
    pStartCodes :: NE.NonEmpty Int,
    -- | Lexer layouts
    pLayouts :: [Layout],
    -- | Current span of token
    pSpan :: Span
  }

newtype ParserM a = ParserM
  { unP :: StateT ParseState (Except Diagnostic) a
  }
  deriving (Functor, Applicative, Monad, MonadState ParseState, MonadError Diagnostic)

initParse :: FilePath -> BS.ByteString -> [Int] -> ParseState
initParse filepath text codes =
  ParseState
    { pFilepath = filepath,
      pInput = AlexInput (startPos filepath) '\n' text,
      pSpan = Span (startPos filepath) (startPos filepath),
      pStartCodes = NE.fromList (codes ++ [0]),
      pLayouts = []
    }

runP :: FilePath -> BS.ByteString -> [Int] -> ParserM a -> Either Diagnostic a
runP filepath text codes parse =
  let st = initParse filepath text codes
      parse' = unP parse
   in runExcept (evalStateT parse' st)

getInput :: ParserM AlexInput
getInput = gets pInput

setInput :: AlexInput -> ParserM ()
setInput input = modify go
  where
    position' = lPos . pInput
    go st =
      st
        { pInput = input,
          pSpan = Span (position' st) (lPos input)
        }

-- | Location functions
position :: ParserM Position
position = gets (lPos . pInput)

location :: ParserM Span
location = gets pSpan

getColumn :: ParserM Int
getColumn = gets (posCol . lPos . pInput)

located :: a -> ParserM (Loc a)
located a = Loc a <$> location

-- | Layout Functions
data Layout = ExplicitLayout | LayoutColumn Int
  deriving (Eq, Show, Ord)

getLayout :: ParserM (Maybe Layout)
getLayout = gets (fmap fst . uncons . pLayouts)

pushLayout :: Layout -> ParserM ()
pushLayout i = modify go
  where
    go st = st {pLayouts = i : pLayouts st}

popLayout :: ParserM ()
popLayout = modify go
  where
    go st =
      st
        { pLayouts = pop (pLayouts st)
        }
    pop = \case
      (_ : xs) -> xs
      [] -> []

-- | Start code Functions
getStartCode :: ParserM Int
getStartCode = gets (NE.head . pStartCodes)

pushStartCode :: Int -> ParserM ()
pushStartCode code = modify go
  where
    go st =
      st
        { pStartCodes = NE.cons code $ pStartCodes st
        }

popStartCode :: ParserM ()
popStartCode = modify go
  where
    go st =
      st
        { pStartCodes = pop (pStartCodes st)
        }
    pop = \case
      _ NE.:| [] -> 0 NE.:| []
      _ NE.:| (x : xs) -> x NE.:| xs

-- | Errors
makeLexerError :: [Snippet] -> ParserM a
makeLexerError snippets = throwError (Diagnostic Error Lexer snippets)

invalidLexemeError :: Position -> ParserM a
invalidLexemeError sp = makeLexerError [Snippet (Span sp sp) "Invalid lexeme"]
