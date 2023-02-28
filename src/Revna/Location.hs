{-# LANGUAGE StandaloneKindSignatures #-}

module Revna.Location
  ( Position (..),
    Span (..),
    Located (..),
    Loc (..),
    startPos,
    nextLine,
    nextColumn,
    movePos,
  )
where

import Data.Kind (Constraint, Type)

data Position = Position
  { -- | absolute offset
    posOffset :: !Int,
    -- | line number of position
    posLine :: !Int,
    -- | column number of position
    posCol :: !Int,
    -- | absolute file path of the file
    posFilepath :: !FilePath
  }
  deriving (Show, Eq)

data Span = Span
  { spanStart :: !Position,
    spanEnd :: !Position
  }
  deriving (Show, Eq)

data Loc a = Loc a Span
  deriving (Show)

startPos :: FilePath -> Position
startPos = Position 0 1 1

nextLine :: Int -> Position -> Position
nextLine x pos = pos {posLine = posLine pos + x}

nextColumn :: Int -> Position -> Position
nextColumn x pos = pos {posCol = posCol pos + x}

movePos :: Position -> Char -> Position
movePos pos '\n' =
  pos {posOffset = posOffset pos + 1, posLine = posLine pos + 1, posCol = 1}
movePos pos _ =
  pos {posOffset = posOffset pos + 1, posCol = posCol pos + 1}

type Located :: (Type -> Type) -> Constraint
class Located f where
  locate :: f b -> Span
  unlocated :: f b -> b

instance Located Loc where
  locate (Loc _ s) = s
  unlocated (Loc b _) = b
