module Parser.ParseError
  ( ParseError(..)
  , expError
  , msgError
  ) where

import Types.Pos

import Data.Foldable
import Data.List

data ErrorDescriptor = Expected String
                     | Message String

data ParseError = ParseError
  { errorPos  :: Pos
  , errorDesc :: [ErrorDescriptor]
  }

expError :: Pos -> String -> ParseError
expError pos desc = ParseError pos [Expected desc]
msgError :: Pos -> String -> ParseError
msgError pos msg = ParseError pos [Message msg]

instance Eq ErrorDescriptor where
  Expected e1 == Expected e2 = e1 == e2
  Message  m1 ==  Message m2 = m1 == m2
  _ == _ = False

instance Semigroup ParseError where
  e1@(ParseError p1 m1) <> e2@(ParseError p2 m2)
    | p2 > p1 || null m1 = e2
    | p1 > p2 || null m2 = e1
    | otherwise = ParseError p1 $ m1 `union` m2

instance Eq ParseError where
  ParseError p1 _ == ParseError p2 _ = p1 == p2
  ParseError p1 _ /= ParseError p2 _ = p1 /= p2

instance Ord ParseError where
  ParseError p1 _ <  ParseError p2 _ = p1 < p2
  ParseError p1 _ >  ParseError p2 _ = p1 > p2
  ParseError p1 _ <= ParseError p2 _ = p1 <= p2
  ParseError p1 _ >= ParseError p2 _ = p1 >= p2

  max p1 p2 = p1 <> p2
  min _ _ = error "Parser.ParseError: incomplete Ord instance"

instance Show ParseError where
  show (ParseError pos []) = show pos <> ": parse error"
  show (ParseError pos msgs) = expectmsg expects <> messages msgs
    where
      expectmsg [] = ""
      expectmsg [e] = show pos <> ": expecting " <> e <> "\n"
      expectmsg [e1, e2] =
        fold [show pos,": expecting either ",e1," or ",e2,"\n"]
      expectmsg (first:rest) =
        fold [show pos,": expecting one of: ",first,expectlist rest,"\n"]
          where expectlist [] = ""
                expectlist [e] = ", or " <> e
                expectlist (e:es) = ", " <> e <> expectlist es
                {-# INLINE expectlist #-}

      -- Some kind of 'filter isExpected' then 'fold' of their contents
      expects = go msgs
        where go [] = []
              go (Expected e:es) = e : go es
              go (Message _:ms)  = go ms
              {-# INLINE go #-}

      messages [] = []
      messages (Expected _:es) = messages es
      messages (Message m:ms) = show pos <> ": " <> m <> "\n" <> messages ms
