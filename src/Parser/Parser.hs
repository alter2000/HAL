module Parser.Parser where
-- TODO: TemplateHaskell QQ?

import Data.Char
import Data.Function
import Control.Applicative
import Control.Monad

import Types.Pos
import Parser.ParseError

-- Types {{{
-- | Wrapper type for our parser, just '(->)' 'Derivs'.
newtype Parser d v = Parser { runParser :: d -> Result d v }

-- | Base typeclass linking 'Parser' and the resulting constructs.
-- Only these primitive elements need to be implemented in order to get
-- the following combinators for free.
class Derivs d where
  dvPos  :: d -> Pos
  dvChar :: d -> Result d Char

data Result d v = Parsed v d ParseError
                | NoParse ParseError

instance Derivs d => Functor (Result d) where
  fmap f (Parsed v d pe) = Parsed (f v) d pe
  fmap _ (NoParse    pe) = NoParse pe

instance Derivs d => Functor (Parser d) where
  fmap f (Parser p) = Parser $ fmap f . p

instance Derivs d => Applicative (Parser d) where
  pure x = Parser $ Parsed x <*> nullError
  (Parser p1) <*> (Parser p2) = Parser $ \ds -> case p1 ds of
    Parsed f d _ -> f <$> p2 d
    NoParse pe -> NoParse pe

instance Derivs d => Alternative (Parser d) where
  empty = Parser $ \ds -> NoParse $ msgError (dvPos ds) "failed alternative"

  Parser p1 <|> Parser p2 = Parser $ \ds -> case p1 ds of
    r@Parsed{} -> r
    NoParse e1 -> case p2 ds of
      Parsed v r e2 -> Parsed v r $ e1 <> e2
      NoParse    e2 -> NoParse    $ e1 <> e2

instance Derivs d => Monad (Parser d) where
  return = pure
  Parser p1 >>= f = Parser $ \ds -> case p1 ds of
    Parsed val r e1 -> case runParser (f val) r of
      Parsed v r' e2 -> Parsed v r' $ e1 <> e2
      NoParse     e2 -> NoParse     $ e1 <> e2
    NoParse e -> NoParse e

instance Derivs d => MonadFail (Parser d) where
  fail [] = Parser $ NoParse . nullError
  fail msg = Parser (\ds -> NoParse (msgError (dvPos ds) msg))


-- | Obtain 'Derivs' for the current position.
getDerivs :: Derivs d => Parser d d
getDerivs = Parser $ join Parsed <*> nullError

-- | Get the current position.
getPos :: Derivs d => Parser d Pos
getPos = Parser $ (dvPos >>= Parsed) <*> nullError

-- | Change 'Derivs' from the current position onwards.
setDerivs :: Derivs d => d -> Parser d ()
setDerivs ds = Parser $ Parsed () ds . nullError

-- }}}

-- Parsing combinators {{{

-- | @satisfy p pred@ consumes and returns the result of @p@ if it satisfies
-- @pred@.
satisfy :: Derivs d => Parser d v -> (v -> Bool) -> Parser d v
satisfy (Parser p) test = Parser $ \ds -> case p ds of
  r@(Parsed v _ _) -> if test v then r else NoParse $ nullError ds
  n@NoParse{} -> n

-- | @followedBy p@ succeeds if @p@ succeeds, without consuming any input.
followedBy :: Derivs d => Parser d v -> Parser d v
followedBy (Parser p) = Parser $ \ds -> case p ds of
  Parsed v _ _ -> Parsed v ds $ nullError ds
  err -> err

-- | @notFollowedBy p@ fails if @p@ succeeds, without consuming any input.
notFollowedBy :: Derivs d => Parser d v -> Parser d ()
notFollowedBy (Parser p) = Parser $ \ds -> case p ds of
  Parsed{}  -> NoParse $ nullError ds
  NoParse{} -> Parsed () ds $ nullError ds

nullError :: Derivs d => d -> ParseError
nullError ds = ParseError (dvPos ds) []

eofError :: Derivs d => d -> ParseError
eofError ds = msgError (dvPos ds) "end of input"

-- | @expected "item"@ fails with @'Types.ParseError.Expected' "item"@.
expected :: Derivs d => String -> Parser d v
expected desc = Parser (\ds -> NoParse $ expError (dvPos ds) desc)

-- | @unexpected "item"@ fails with @'Types.ParseError.Message' "unexpected item"@.
unexpected :: Derivs d => String -> Parser d v
unexpected = fail . ("unexpected " <>)

--- }}}

-- Annotations {{{
-- | Annotates a parser with what to be parsed.
-- The resulting parser yields an 'Types.ParseError.Expected' error message if
-- parse is unsuccessful and no error information is already available
-- indicating a position farther right in the source code.
infixl 1 <?>
(<?>) :: Derivs d => Parser d v -> String -> Parser d v
(Parser p) <?> desc = Parser $ go <*> p
  where
    go ds (Parsed v r err) = Parsed v r $ insert ds err
    go ds (NoParse err)    = NoParse    $ insert ds err
    insert ds err@(ParseError pos _)
      | pos > dvPos ds = err
      | otherwise = expError (dvPos ds) desc

-- | Like '(<?>)' but unconditionally overrides existing error information.
infixl 1 <?!>
(<?!>) :: Derivs d => Parser d v -> String -> Parser d v
(Parser p) <?!> desc = Parser $ go <*> p
  where
    go ds (Parsed v r err) = Parsed v r $ insert ds err
    go ds (NoParse err)    = NoParse    $ insert ds err
    insert ds ParseError{} = expError (dvPos ds) desc
-- }}}

-- Char parsers {{{
-- | Matches any single character.
anyChar :: Derivs d => Parser d Char
anyChar = Parser dvChar

-- | @char c@ matches only @c@.
char :: Derivs d => Char -> Parser d Char
char ch = satisfy anyChar (== ch) <?> show ch

-- | @oneOf s@ matches any character in @s@.
oneOf :: Derivs d => String -> Parser d Char
oneOf chs = satisfy anyChar (`elem` chs)
      <?> ("any character in " ++ show chs)

-- | @noneOf s@ matches any character not in @s@.
noneOf :: Derivs d => String -> Parser d Char
noneOf chs = satisfy anyChar (`notElem` chs)
       <?> ("any character not in " ++ show chs)

-- | @string s@ matches all the characters in @s@ in sequence.
string :: Derivs d => String -> Parser d String
string s = p s <?> show s
  where p = foldr ((*>) . char) (pure [])

-- | Match any letter.
letter :: Derivs d => Parser d Char
letter = satisfy anyChar isAlpha <?> "letter"

-- | Match any letter or digit.
alphaNum :: Derivs d => Parser d Char
alphaNum = satisfy anyChar isAlphaNum <?> "letter or digit"

-- | Match any digit.
digit :: Derivs d => Parser d Char
digit = satisfy anyChar isDigit <?> "digit"

-- | Match any hexadecimal digit.
hexDigit :: Derivs d => Parser d Char
hexDigit = satisfy anyChar isHexDigit <?> "hexadecimal digit (0-9, a-f)"

-- | Match a newline.
newline :: Derivs d => Parser d Char
newline = char '\n'

-- | Match any whitespace character.
space :: Derivs d => Parser d Char
space = satisfy anyChar isSpace <?> "whitespace character"

-- | Match a sequence of zero or more whitespace characters.
spaces :: Derivs d => Parser d String
spaces = many space

-- | Match the end of file.
eof :: Derivs d => Parser d ()
eof = notFollowedBy anyChar <?> "end of input"
-- }}}
