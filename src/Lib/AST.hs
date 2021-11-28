{-# OPTIONS_GHC -fmax-pmcheck-models=2000 #-}
-- | HALP NEED EXPLAIN
--
-- * Single expression -> StateT (Interp)
--   * AST' -> Scope AST'
module Lib.AST
  where


import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import Control.Arrow
import Data.Function
import Data.Foldable

import qualified Data.Map as M
import Control.Exception

import Types.Exceptions
import RecursionSchemes

import Types.AST
import Types.Interp
import Types.Pos

runStep :: AST' -> Env -> IO (AST', Env)
runStep = applyRewriteRules >>> revCataM alg >>> runInterp >=> resolveScope

-- Algebra {{{
alg :: ASTF AST' -> Interp AST'
alg (Atom a) = findAtom a
alg  (Int a) = pure $ int a
alg (Bool a) = pure $ bool a
alg  (Str a) = pure $ str a
alg (DottedList as t) = pure $ dlist as t
alg (Lambda ps b c) = pure $ func ps b c
alg (Builtin b) = pure $ mkBuiltin b

alg (List [Fix(Atom "quote"), Fix(List [])]) = pure $ mkQuote $ list []
alg (List [Fix(Atom "quote"), a]) = pure a

alg (List [Fix(Atom "define"), Fix var, Fix def]) = do
  isParams var
  def' <- outF <$> alg def
  evalCtx [var] [def'] var
  pure (Fix var)
alg (List [Fix(Atom "lambda"), Fix(List params), body]) =
  func (getAtom . outF <$> params) body <$> pullEnv

alg (List [Fix(Atom "cdr"), Fix(List [Fix(Atom "quote"),
  Fix(List (_:xs))])]) = pure $ list xs
alg (List [Fix(Atom "cdr"), Fix arg@(List (x:xs))]) = case outF x of
  Atom  _ -> alg arg >>= \v -> alg . List $ [atom "cdr", v]
  _ -> pure $ list xs

alg (List [Fix(Atom "car"), Fix(List [Fix(Atom "quote"),
  Fix(List (x:_))])]) = pure x
alg (List [Fix(Atom "car"), Fix arg@(List (x:_))]) = case outF x of
  Atom  _ -> alg arg >>= \v -> alg . List $ [atom "car", v]
  _ -> pure x

alg (List (Fix(Builtin (Func a)):as)) = a as
alg (List (Fix x : xs)) = alg x >>= \var -> case outF var of
  (Builtin (Func f)) -> f xs
  l@Lambda{} -> do
    applyLambda l xs
  x' -> throw $ TypeMismatch nPos $ show x' <> ": not a function"
alg (List []) = throw $ TypeMismatch nPos "cannot eval empty list"

applyLambda :: ASTF (Fix ASTF) -> [Fix ASTF] -> Interp AST'
applyLambda (Lambda args body localEnv) xs = local (<> localEnv)
  $ applyCtx (Atom <$> args) (outF <$> xs) (outF body)
applyLambda _ _ = error "Lib.AST.applyLambda: unreachable code (not lambda)"
-- }}}

-- Helpers {{{
applyCtx :: [ASTF AST'] -> [ASTF AST'] -> ASTF AST' -> Interp AST'
applyCtx vars defs body = local (<> newEnv vars defs) (alg body)

evalCtx :: [ASTF AST'] -> [ASTF AST'] -> ASTF AST' -> Interp AST'
evalCtx vars defs body = do
  _defs' <- fmap outF <$> traverse alg defs
  modify (<> newEnv vars defs)
  (_oldEnv, prevEnv) <- liftA2 (,) get ask
  local (<> prevEnv) (alg body)

isParams :: ASTF AST' -> Interp (ASTF AST')
isParams (Atom a) = pure $ Atom a
isParams (DottedList xs b) = do
  pxs <- filterM checkAtom $ outF <$> xs
  pb <- isParams $ outF b
  pure $ DottedList (Fix <$> pxs) (Fix pb)
    where checkAtom Atom{} = pure True
          checkAtom _ = throw $ TypeMismatch nPos "#rest: expected atom"
isParams _ = throw $ TypeMismatch nPos "define: expected atom"

getAtom :: ASTF a -> VarName
getAtom (Atom a) = a
getAtom _ = throw $ TypeMismatch nPos "Env: expected atom"

findAtom :: VarName -> Interp AST'
findAtom a = maybe (throw $ UnboundVar nPos a)
  pure . M.lookup a . getEnv =<< liftA2 (<>) get ask

newEnv :: [ASTF a] -> [ASTF AST'] -> Env
newEnv vars = packEnv . zipWith ((,) . getAtom) vars
  where packEnv = Env . fmap Fix . M.fromList

pullEnv :: Interp Env
pullEnv = liftA2 (<>) get ask

printEnv :: Interp ()
printEnv = pullEnv >>= liftIO
  . (putStrLn . ("ENV:\t" <>) . show . Env)
  . (`M.difference` builtins) . getEnv

-- }}}

-- rewrite rules {{{
 -- [x] `(define (func p) expr)` -> `(define func (lambda (p) (expr)))`
 -- [x] `(let ((a 1) (b 2)) (+ a b))` -> `((lambda (a b) (+ a b)) 1 2)`
applyRewriteRules :: AST' -> AST'
applyRewriteRules = rewrite . outF

rewrite :: ASTF AST' -> AST'
rewrite (List [Fix(Atom "define"), Fix(List (Fix(Atom fname) : args)), body])
  = list [atom "define", atom fname, list [atom "lambda", list args, body]]
rewrite (List [Fix(Atom "let"), Fix(List pairs), body])
  = list $ [list [atom "lambda", list $ fst <$> unzipList pairs
                 , body]] <> fmap snd (unzipList pairs)
rewrite (List (Fix(Atom a):as)) = list $ maybe (atom a:as)
  ((: as) . mkBuiltin) $ M.lookup a builtins
rewrite a = Fix a

unzipList :: [AST'] -> [(AST', AST')]
unzipList [] = []
unzipList (Fix(List [a, b]):xs) = (a, b) : unzipList xs
unzipList (_:xs) = unzipList xs
-- }}}

-- Builtins {{{
builtins :: M.Map VarName (Func Interp)
builtins = Func <$> M.fromList
  [ ("quote" , quote)
  , ("car"   , car . fmap outF)
  , ("cdr"   , cdr . fmap outF)
  , ("cons"  , cons)
  , ("atom?" , isAtom . fmap outF)
  , ("cond"  , cond . fmap outF)
  , ("+"     , varOp (wrap int (+) getInt) (int 0))
  , ("-"     , varOp (wrap int (-) getInt) (int 0))
  , ("*"     , varOp (wrap int (*) getInt) (int 1))
  , ("/"     , binOp (wrap int div getInt))
  , ("div"   , binOp (wrap int div getInt))
  , ("mod"   , binOp (wrap int mod getInt))
  , ("eq?"   , binOp (eqAll `on` outF))
  , ("="     , boolOp2 getInt (==))
  , ("<"     , boolOp2 getInt (<))

  , ("display", display putStr)
  ]

wrap :: Applicative m => (base -> up)       -- ^ wrapper to call in the end
                      -> (b2 -> b2 -> base) -- ^ binary operation
                      -> (p -> m b2)        -- ^ unwrapper
                      -> p -> p -> m up
wrap inF op unF = fmap inF ... (liftA2 op `on` unF)
  where k ... m = (k .) . m

primEnv :: Env
primEnv = Env $ mkBuiltin <$> builtins

quote :: [AST'] -> Interp AST'
quote [Fix(List xs)] = pure . list $ atom "quote" : xs
quote [e] = pure . list $ atom "quote" : [e]
quote as  = throw $ BadArguments nPos (length as) 1

cond :: [ASTF AST'] -> Interp AST'
cond ((List [Fix a, Fix b]):rest) = alg a >>= \x -> case outF x of
  Bool  True -> alg b
  Bool False -> cond rest
  _ -> throw $ TypeMismatch nPos "cond only handles bools"
cond _ = throw $ TypeMismatch nPos "cond only handles bools"

-- TODO: really need to handle 'quote'?
car :: [ASTF AST'] -> Interp AST'
car ((List[ Fix(Atom "quote"), Fix(List(r:_)) ]):_) = pure r
car (a'@(List(Fix(Atom _):_)):_) = alg $ List [atom "car", Fix a']
car (   (List(a          :_)):_) = pure a
car _ = pure $ list []

cdr :: [ASTF AST'] -> Interp AST'
-- cdr (Fix    (List[Fix(Atom "quote"), Fix(List(_:r))]):_) = pure $ list r
cdr (a'@(List(Fix(Atom _):_)):_) = alg $ List [atom "cdr", Fix a']
cdr (   (List(          _:r)):_) = pure $ list r
cdr _ = pure $ list []

cons :: [AST'] -> Interp AST'
cons [a, Fix (List bs)] = pure $ list $ a:bs
cons [a, b]             = pure $ list [a, b]
cons as = throw $ BadArguments nPos 2 (length as)

display :: (String -> IO ()) -> [AST'] -> Interp AST'
display pF a = do
  v <- traverse alg (outF <$> a)
  liftIO $ traverse_ (printAST pF) v >> pF "\n"
  pure . mkQuote $ list []

printAST ::(String -> IO ()) -> Fix ASTF -> IO ()
printAST pF (Fix(List[Fix (Atom "quote"), x])) = pF $ '\'' : show x
printAST pF (Fix(Str x)) = pF x
printAST pF x = pF $ show x

isAtom :: [ASTF AST'] -> Interp AST'
isAtom [List[Fix(Atom "quote"), Fix(List [])]] = pure $ bool True
isAtom [List[Fix(Atom "quote"), Fix(Atom _)]] = pure $ bool True
isAtom [a@(Atom _)] = isAtom . (:[]) . outF =<< alg a
isAtom [Int _] = pure $ bool True
isAtom [Bool _] = pure $ bool True
isAtom [Str _] = pure $ bool True
isAtom [DottedList{}] = pure $ bool False
isAtom [a] = isAtom . (:[]) . outF =<< alg a
isAtom l = throw $ BadArguments nPos (length l) 1

type BinOp r = r -> r -> Interp r

varOp :: BinOp AST' -- ^ Folding function
      -> AST' -- ^ Expr to break down
      -> [AST'] -- ^ Arguments
      -> Interp AST'
varOp _ _ [] = throw $ BadArguments nPos 0 2
varOp x _ [a, b] = x a b
varOp x k as = foldlM x k as

binOp :: BinOp AST' -> [AST'] -> Interp AST'
binOp op [a, b] = a `op` b
binOp _ s = throw $ BadArguments nPos (length s) 2

boolOp2 :: (AST' -> Interp a) -> (a -> a -> Bool) -> [AST'] -> Interp AST'
boolOp2 f cmp [a, b] = bool <$> (cmp <$> f a <*> f b)
boolOp2 _ _ s = throw $ BadArguments nPos (length s) 2

getInt :: AST' -> Interp Integer
-- getInt (Fix (List [s])) = getInt s
-- getInt (Fix (DottedList [] s)) = getInt s
-- getInt (Fix (DottedList [s] _)) = getInt s
-- getInt (Fix (Str n)) = maybe 0 fst . listToMaybe $ reads n
getInt (Fix (Int n)) = pure n
getInt (Fix n@(Atom _)) = getInt =<< alg n
getInt (Fix x@List{}) = getInt =<< alg x
getInt _ = throw $ TypeMismatch nPos "not a numeral"

eqAll :: ASTF AST' -> ASTF AST' -> Interp AST'
eqAll (Str a)  (Str b) = pure $ bool $ a == b
eqAll (Bool a)  (Bool b) = pure $ bool $ a == b
eqAll (Int a)  (Int b) = pure $ bool $ a == b
eqAll ((List [Fix(Atom "quote"), Fix(List [])]))
      ((List [Fix(Atom "quote"), Fix(List [])])) = pure $ bool True
eqAll ((List [Fix(Atom "quote"), Fix(List _)]))
      ((List [Fix(Atom "quote"), Fix(List _)])) = pure $ bool False
eqAll ((List [Fix(Atom "quote"), Fix(Atom a)]))
      ((List [Fix(Atom "quote"), Fix(Atom b)])) = pure $ bool $ a == b
-- eqAll ((List [Fix(Atom "quote"), Fix a]))
--       ((List [Fix(Atom "quote"), Fix b])) = eqAll a b
eqAll DottedList{} DottedList{} = pure $ bool False
eqAll a b = do
  x <- alg a
  y <- alg b
  (eqAll `on` outF) x y
-- }}}
