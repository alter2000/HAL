-- | Simple data type to keep track of character positions
-- within a text file or other text stream.
module Types.Pos where

-- | Basic position indicator.
data Pos = Pos
  { posFile :: !FilePath -- ^ filename
  , posLine :: !Int      -- ^ line number
  , posCol  :: !Int      -- ^ column number
  }

-- | Blank starting postition.
nPos :: Pos
nPos = Pos "" 0 0

-- | @stdin@ starting postition.
stdinNPos :: Pos
stdinNPos = Pos "<stdin>" 0 0

-- | @nextPos c@ incrementally computes the next position analyzing @c@.
-- __NOTE__: hardcoded 8-character tab stops.
nextPos :: Pos -> Char -> Pos
nextPos (Pos file line _) '\n' = Pos file (line + 1) 1
nextPos (Pos file ln col) '\t' = Pos file ln $ ((col + 8 - 1) `div` 8) * 8 + 1
nextPos (Pos file line col)  _ = Pos file line (col + 1)

instance Eq Pos where
  Pos f1 l1 c1 == Pos f2 l2 c2 = f1 == f2 && l1 == l2 && c1 == c2

-- | Ordered by line number, then column number.
instance Ord Pos where
  Pos _ l1 c1 <= Pos _ l2 c2 =
    (l1 < l2) || (l1 == l2 && c1 <= c2)

-- | Like Clang: @<file>:<line>:<column>@
instance Show Pos where
  show (Pos file line col) = concat [file, ":", show line, ":", show col]

-- | Show a position relative to a base position.
-- Hides elements which are the same
showPosRel :: Pos -> Pos -> [Char]
showPosRel (Pos f1 l1 _col) (Pos f2 l2 col)
  | f1 == f2 = if l1 == l2
      then "column " <> show col
      else "line " <> show l2 <> ", column " <> show col
  | otherwise = show $ Pos f2 l2 col
