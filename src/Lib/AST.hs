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
cond ((List [Fix a, b]):rest) = alg a >>= \x -> case x of
  Fix (Bool True) -> pure b
  Fix (Bool False) -> cond rest
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
eqAll ((List [Fix(Atom "quote"), Fix(Atom a)]))
      ((List [Fix(Atom "quote"), Fix(Atom b)])) = pure $ bool $ a == b
eqAll DottedList{} DottedList{} = pure $ bool False
eqAll a b = do
  x <- alg a
  y <- alg b
  (eqAll `on` outF) x y
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

