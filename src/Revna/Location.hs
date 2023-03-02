module Revna.Location
  ( Position (..),
    Span (..),
    HasSpan (..),
    Loc (..),
    startPos,
    nextLine,
    nextColumn,
    unlocated,
    movePos,
    withSpan1,
    withSpan2,
  )
where

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
  deriving (Show, Eq, Ord)

data Span = Span
  { spanStart :: !Position,
    spanEnd :: !Position
  }
  deriving (Show, Eq)

data Loc a = Loc a Span
  deriving (Show)

instance Semigroup Span where
  (Span s1 e1) <> (Span s2 e2) = Span (min s1 s2) (max e1 e2)

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

class HasSpan a where
  getSpan :: a -> Span

instance HasSpan (Loc a) where
  getSpan (Loc _ s) = s

unlocated :: Loc a -> a
unlocated (Loc b _) = b

withSpan1 :: HasSpan a => a -> Span
withSpan1 = getSpan

withSpan2 :: (HasSpan a, HasSpan b) => a -> b -> Span
withSpan2 a b = getSpan a <> getSpan b
