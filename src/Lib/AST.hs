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
import Data.Maybe
import Control.Exception

import Types.Exceptions
import RecursionSchemes

import Types.AST
import Types.Interp
import Types.Pos

-- evalTerm :: AST' -> Env -> InputT IO (AST', Env)
-- evalTerm ast e = liftIO $ catch (runStep ast e)
--   (\ex -> halExcept ex >> pure (list [], e))

-- runStep :: AST' -> Env -> IO (AST', Env)
-- runStep = applyRewriteRules >>> cataMCps alg >>> runInterp >=> resolveScope

runStep :: AST' -> Env -> IO (AST', Env)
runStep = applyRewriteRules >>> revCataM alg >>> runInterp >=> resolveScope

-- Algebra {{{
-- | TODO: 'RMAlg ASTF Interp AST\''?
alg :: ASTF AST' -> Interp AST'
alg  (Int a) = pure $ int a
alg (Bool a) = pure $ bool a
alg  (Str a) = pure $ str a
alg (DottedList as t) = pure $ dlist as t
alg (Lambda ps b c) = pure $ func ps b c
alg (Builtin b) = pure $ mkBuiltin b
alg (Atom a) = findAtom a
alg (List [Fix(Atom "quote"), a]) = pure a
alg (List [Fix(Atom "define"), Fix var, Fix def]) =
  retAtom var >> evalCtx [var] [def] var >> pure (Fix var)
alg (List [Fix(Atom "lambda"), Fix(List params), body]) =
  func (getAtom . outF <$> params) body <$> liftA2 (<>) get ask

alg (List (Fix(Builtin (Func a)):as)) = a as
alg (List (Fix x : xs)) = alg x >>= \(Fix var) -> case var of
    (Builtin (Func f)) -> f =<< mapM alg (outF <$> xs)
    l@Lambda{} -> applyLambda l xs
    x' -> do
      env <- liftA2 (<>) get ask
      liftIO (putStrLn $ "IN LAST MATCH: " <> show x' )
      liftIO (putStrLn $ "ENV: " <> show env)
      local (<> env) $ alg x'
alg (List []) = pure $ list []

applyLambda :: ASTF (Fix ASTF) -> [Fix ASTF] -> Interp AST'
applyLambda (Lambda args body localEnv) xs = do
  (globalEnv, prevEnv) <- liftA2 (,) get ask
  vs <- mapM alg (outF <$> xs)
  v <- local (<> globalEnv <> localEnv <> prevEnv) $
             evalCtx (Atom <$> args) (outF <$> vs) (outF body)
  modify (const globalEnv)
  pure v
applyLambda _ _ = error "Lib.AST.applyLambda: unreachable code (not lambda)"
-- }}}

-- Helpers {{{
-- applyCtx :: [ASTF AST'] -> [ASTF AST'] -> ASTF AST' -> Interp AST'
-- applyCtx vars defs v = local (<> newEnv vars defs) (alg v)

evalCtx :: [ASTF AST'] -> [ASTF AST'] -> ASTF AST' -> Interp AST'
evalCtx vars defs v = do
  -- oldestEnv <- get
  modify (<> newEnv vars defs)
  (oldEnv, prevEnv) <- liftA2 (,) get ask
  -- liftIO $ print prevEnv
  -- liftIO $ print oldEnv
  modify (<> prevEnv)
  v' <- alg v
  modify (const oldEnv)
  pure v'

retAtom :: ASTF a -> Interp (ASTF a)
retAtom (Atom a) = pure $ Atom a
retAtom _ = throw $ TypeMismatch nPos "expected atom"

getAtom :: ASTF a -> VarName
getAtom (Atom a) = a
getAtom _ = throw $ TypeMismatch nPos "expected atom"

findAtom :: VarName -> Interp AST'
findAtom a = maybe (throw $ UndefinedSymbol nPos a)
  pure . M.lookup a . getEnv =<< liftA2 (<>) get ask

newEnv :: [ASTF a] -> [ASTF AST'] -> Env
newEnv vars = packEnv . zipWith ((,) . getAtom) vars
  where packEnv = Env . fmap Fix . M.fromList

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
-- rewrite (List [Fix(List[Fix(Atom "lambda"), Fix(List args), Fix(List bd)])])
--   = gets $ Fix . Lambda (getAtom . outF <$> args) bd
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
builtins = Func <$> M.fromList [
    ("quote", quote)
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("atom?", isAtom)
  , ("cond", cond)
  , ("+",   varOp (int 0) (wrap int (+) getInt))
  , ("-",   varOp (int 0) (wrap int (-) getInt))
  , ("*",   varOp (int 1) (wrap int (*) getInt))
  , ("/",   binOp (wrap int div getInt))
  , ("div", binOp (wrap int div getInt))
  , ("mod", binOp (wrap int mod getInt))
  , ("eq?", binOp eqAll)
  , ("=",   boolOp2 getInt (==))
  , ("<",   boolOp2 getInt (<))
  ]

primEnv :: Env
primEnv = Env $ mkBuiltin <$> builtins

quote :: [AST'] -> Interp AST'
quote [Fix(List xs)] = pure . list $ atom "quote" : xs
quote [e] = pure . list $ atom "quote" : [e]
quote as  = throw $ BadArguments nPos (length as) 1

cond :: [AST'] -> Interp AST'
cond (Fix (List [Fix a, b]):rest) = alg a >>= \x -> case x of
  Fix (Bool True) -> pure b
  Fix (Bool False) -> cond rest
  _ -> throw $ TypeMismatch nPos "cond only handles bools"
cond _ = throw $ TypeMismatch nPos "cond only handles bools"

-- TODO: really need to handle 'quote'?
car :: [AST'] -> Interp AST'
car (Fix(List[ Fix(Atom "quote"), Fix(List(r:_)) ]):_) = pure r
car (Fix a'@(List(Fix(Atom _):_)):_) = alg $ List [atom "car", Fix a']
car (Fix    (List(a          :_)):_) = pure a
car _ = pure $ list []

cdr :: [AST'] -> Interp AST'
-- cdr (Fix    (List[Fix(Atom "quote"), Fix(List(_:r))]):_) = pure $ list r
cdr (Fix a'@(List(Fix(Atom _):_)):_) = alg $ List [atom "cdr", Fix a']
cdr (Fix    (List(_          :r)):_) = pure $ list r
cdr _ = pure $ list []

cons :: [AST'] -> Interp AST'
cons [a, Fix (List bs)] = pure $ list $ a:bs
cons [a, b]             = pure $ list [a, b]
cons as = throw $ BadArguments nPos 2 (length as)

isAtom :: [AST'] -> Interp AST'
isAtom [Fix(List[Fix(Atom "quote"), Fix(List [])])] = pure $ bool True
isAtom [Fix(List[Fix(Atom "quote"), Fix(Atom _)])] = pure $ bool True
isAtom [Fix a@(Atom _)] = isAtom . (:[]) =<< alg a
isAtom [Fix (Int _)] = pure $ bool True
isAtom [Fix (Bool _)] = pure $ bool True
isAtom [Fix (Str _)] = pure $ bool True
isAtom [Fix DottedList{}] = pure $ bool False
isAtom [Fix a] = isAtom . (:[]) =<< alg a
isAtom l = throw $ BadArguments nPos (length l) 1

type BinOp r = r -> r -> Interp r

varOp :: AST' -- ^ Expr to break down
      -> BinOp AST' -- ^ Folding function
      -> [AST'] -- ^ Arguments
      -> Interp AST'
varOp _ _ [] = throw $ BadArguments nPos 0 2
varOp _ x [a, b] = x a b
varOp k x as = foldlM x k as

binOp :: BinOp AST' -> [AST'] -> Interp AST'
binOp op [a, b] = a `op` b
binOp _ s = throw $ BadArguments nPos (length s) 2

boolOp2 :: (AST' -> a) -> (a -> a -> Bool) -> [AST'] -> Interp AST'
boolOp2 f cmp [a, b] = pure $ bool $ f a `cmp` f b
boolOp2 _ _ s = throw $ BadArguments nPos (length s) 2

getInt :: AST' -> Integer
getInt (Fix (List [s])) = getInt s
getInt (Fix (DottedList [] s)) = getInt s
getInt (Fix (DottedList [s] _)) = getInt s
getInt (Fix (Int n)) = n
getInt (Fix (Str n)) = maybe 0 fst . listToMaybe $ reads n
getInt _ = throw $ TypeMismatch nPos "not a numeral"

wrap :: (base -> result) -> (base' -> base' -> base)
     -> (obj -> base') -> obj -> obj -> Interp result
wrap w op up = (pure .) . (w .) . (op `on` up)

eqAll :: AST' -> AST' -> Interp AST'
eqAll (Fix  (Str a))  (Fix  (Str b)) = pure $ bool $ a == b
eqAll (Fix (Bool a))  (Fix (Bool b)) = pure $ bool $ a == b
eqAll (Fix  (Int a))  (Fix  (Int b)) = pure $ bool $ a == b
eqAll (Fix (List [Fix(Atom "quote"), Fix(List [])]))
      (Fix (List [Fix(Atom "quote"), Fix(List [])])) = pure $ bool True
eqAll (Fix (List [Fix(Atom "quote"), Fix(Atom a)]))
      (Fix (List [Fix(Atom "quote"), Fix(Atom b)])) = pure $ bool $ a == b
eqAll (Fix DottedList{}) (Fix DottedList{}) = pure $ bool False
eqAll (Fix a) (Fix b) = do
  x <- alg a
  y <- alg b
  eqAll x y
-- }}}

-- alg all'@(List (Fix(Lambda vars (Fix body) env): vals)) = do
--   gEnv <- get
--   liftIO $ putStrLn $ "IN LAMBDA APPLICATION\t" <> show vars <> show body <> show vals
--   let env' = env <> newEnv (Atom <$> vars) (outF <$> vals)
--   v <- local (env' <>) (alg body)
--   v' <- evalCtx (Atom <$> vars) (outF <$> vals) body
--   liftIO $ putStrLn $ "GIVEN BODY:\t" <> show body
--   liftIO $ putStrLn $ "APPLIED LAMBDA:\tREADER:\t" <> show v <> "\tSTATE:\t" <> show v'
--   modify (const gEnv)
--   pure v

