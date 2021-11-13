module Parser.AST
  -- ( 
  -- )
  where

import Control.Applicative
import Data.Functor
import Data.Maybe
import Data.Foldable

import Types.AST
import Types.Pos
import Parser.Parser
import Parser.ParseError

data ASTDerivs = ASTDerivs
  { adAtom   :: Result ASTDerivs AST'
  , adInt    :: Result ASTDerivs AST'
  , adBool   :: Result ASTDerivs AST'
  , adString :: Result ASTDerivs AST'
  , adList   :: Result ASTDerivs AST'
  , adQuote  :: Result ASTDerivs AST'

  , adIgnore :: Result ASTDerivs String
  , adElem  :: Result ASTDerivs AST'

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
    , adBool   = pBool d
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
  where P pExpr = P adIgnore *> P pElem

parseFile :: FilePath -> String -> Either ParseError [AST']
parseFile fname s = case pExpr $ evalDerivs (Pos fname 1 1) s of
    Parsed v _ _ -> Right v
    NoParse e -> Left e
  where P pExpr = P adIgnore *>
          some (P pElem <* P adIgnore) <* eof

pIgnore :: ASTDerivs -> Result ASTDerivs String
P pIgnore = concat <$>
    (optional spaces *> many (P pComment <* spaces))
              <?> "non-code"

pElem :: ASTDerivs -> Result ASTDerivs AST'
P pElem = asum [P adBool, P adInt, P adString, P adQuote, P adAtom, P adList]

-- | Atoms are strings of any non-whitespace character starting with a letter
pAtom :: ASTDerivs -> Result ASTDerivs AST'
P pAtom = do
  prefix <- some $ letter <|> oneOf "?!$%^&*_+-=,.#"
  middle <- many $ alphaNum <|> oneOf "?!$%^&*_+-=#,./"
  suffix <- optional $ char '\''
  let tok = prefix <> middle <> maybeToList suffix
  if tok == "#f" || tok == "#t" then unexpected "boolean" <?> "atom"
                                else pure (atom tok) <?> "atom"

pInt :: ASTDerivs -> Result ASTDerivs AST'
P pInt = int . read <$> some digit <?> "integer"

pBool :: ASTDerivs -> Result ASTDerivs AST'
P pBool = string "#f" $> bool False
      <|> string "#t" $> bool True
      <?> "boolean"

pString :: ASTDerivs -> Result ASTDerivs AST'
P pString = str <$> (char '"' `around` many (noneOf "\"\n")) <?> "string"

pList :: ASTDerivs -> Result ASTDerivs AST'
P pList = list <$>
  (char '(' *> P adIgnore `around` sepBy (P adElem) (P adIgnore) <* char ')')
  -- <|> TODO: dottedList
  <?> "list"

pQuote :: ASTDerivs -> Result ASTDerivs AST'
P pQuote = quote <$> (char '\'' *> (P adList <|> P adAtom)) <?> "quote"

pComment :: ASTDerivs -> Result ASTDerivs String
P pComment = char ';' *> many (noneOf "\n") <?> "comment"
