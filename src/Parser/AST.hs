module Parser.AST
  where

import Control.Applicative
import Data.Functor
import Data.Foldable

import Types.AST
import RecursionSchemes
import Types.Pos
import Parser.Parser
import Parser.ParseError

data ASTDerivs = ASTDerivs
  { adAtom   :: Result ASTDerivs AST'
  , adInt    :: Result ASTDerivs AST'
  -- , adBool   :: Result ASTDerivs AST'
  , adString :: Result ASTDerivs AST'
  , adList   :: Result ASTDerivs AST'
  , adQuote  :: Result ASTDerivs AST'

  , adIgnore :: Result ASTDerivs String
  , adElem  :: Result ASTDerivs AST'
  , adHash   :: Result ASTDerivs AST'

  , adChar   :: Result ASTDerivs Char
  , adPos    :: Pos
  }

instance Derivs ASTDerivs where
  dvChar = adChar
  dvPos  = adPos

evalDerivs :: Pos -> String -> ASTDerivs
evalDerivs pos s = d where
  d = ASTDerivs
    { adChar = case s of
     (c:s') -> Parsed c (evalDerivs (nextPos pos c) s') $ nullError d
     [] -> NoParse $ eofError d
    , adPos    = pos
    , adAtom   = pAtom d
    , adInt    = pInt d
    , adHash   = pHash d
    , adString = pString d
    , adList   = pList d

    , adQuote  = pQuote d
    , adIgnore = pIgnore d
    , adElem   = pElem d
    }

parse :: FilePath -> String -> Either ParseError AST'
parse fname s = case pExpr $ evalDerivs (Pos fname 1 1) s of
    Parsed v _ _ -> Right v
    NoParse e -> Left e
  where P pExpr = P adIgnore *> P adElem

parseFile :: FilePath -> String -> Either ParseError [AST']
parseFile fname s = case pExpr $ evalDerivs (Pos fname 1 1) s of
    Parsed v _ _ -> Right v
    NoParse e -> Left e
  where P pExpr = P adIgnore *> some (P adElem <* P adIgnore) <* eof'

pIgnore :: ASTDerivs -> Result ASTDerivs String
P pIgnore = concat <$>
    (optional spaces *> many (P pComment <* spaces))
              <?> "non-code"

pElem :: ASTDerivs -> Result ASTDerivs AST'
P pElem = asum [P adHash, P adInt, P adString, P adQuote, P adAtom, P adList]

-- | Atoms are strings of alphanumeric characters (+ some symbols)
-- starting with a letter or symbol
pAtom :: ASTDerivs -> Result ASTDerivs AST'
P pAtom = do
  prefix <- some $ letter <|> oneOf "?!$%^&*_+-=#,.<>/"
  middle <- many $ alphaNum <|> oneOf "?!$%^&*_+-=#,.<>/"
  let tok = prefix <> middle
  case tok of
    "." -> unexpected "dotted list" <?> "atom"
    _   -> pure (atom tok) <?> "atom"

{- HLINT ignore "Avoid restricted function" -}
pInt :: ASTDerivs -> Result ASTDerivs AST'
P pInt = int . read <$> some digit <?> "integer"

pHash :: ASTDerivs -> Result ASTDerivs AST'
P pHash = P pBool -- <|> char '#' *> P pCustomNumber/Macro/Etc

pBool :: ASTDerivs -> Result ASTDerivs AST'
P pBool = string "#f" $> bool False <|> string "#t" $> bool True <?> "boolean"

pString :: ASTDerivs -> Result ASTDerivs AST'
P pString = str <$> (char '"' `around` many (noneOf "\"\n")) <?> "string"

pList :: ASTDerivs -> Result ASTDerivs AST'
P pList = parens dottedList
  <|> list <$> parens (P adElem `sepBy` P adIgnore)
  <?> "list"

dottedList :: Parser ASTDerivs AST'
dottedList = do
  xs <- P adElem `sepBy` P adIgnore
  x <- P adIgnore *> char '.' *> P adIgnore `around` P adElem
  case x of
    Fix(List xs') -> pure (list $ xs<>xs') <?> "cons'd dotted list"
    _ -> pure (dlist xs x) <?> "dotted list"

parens :: Parser ASTDerivs a -> Parser ASTDerivs a
parens p = char '(' *> P adIgnore `around` p <* char ')' <?> "parentheses"

pQuote :: ASTDerivs -> Result ASTDerivs AST'
P pQuote = mkQuote <$> (char '\'' *> P adElem) <?> "quote"

pComment :: ASTDerivs -> Result ASTDerivs String
P pComment = char ';' *> many (noneOf "\n") <?> "comment"
