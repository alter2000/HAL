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
import qualified Data.Map as M

import Control.Exception
import System.IO

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

-- | needs more flesh, usable while inside 'Control.Monad.Except.ExceptT'
halExcept :: HALError -> IO ()
halExcept = hPutStrLn stderr . displayException

-- | TODO: 'RMAlg ASTF Interp AST\''?
alg :: ASTF AST' -> Interp AST'
alg  (Int a) = pure $ int a
alg (Bool a) = pure $ bool a
alg  (Str a) = pure $ str a
alg (DottedList as t) = pure $ dlist as t
alg (Lambda ps b c) = pure $ func ps b c
alg (Builtin b) = pure $ mkBuiltin b
alg (Atom a) = maybe (throw $ UndefinedSymbol nPos a)
                      pure . M.lookup a . getEnv =<< liftA2 (<>) get ask

alg (List [Fix(Atom "quote"), a]) = pure a
alg (List [Fix(Atom "define"), Fix var, Fix def]) =
  retAtom var >> evalCtx [var] [def] var >> pure (Fix var)
alg (List [Fix(Atom "lambda"), Fix(List params), body]) =
  func (getAtom . outF <$> params) body <$> liftA2 (<>) get ask

alg (List (Fix(Builtin (Func a)):as)) = a as
alg (List (Fix x : xs)) = do
  (Fix funVar) <- alg x
  case funVar of
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
  v <- local (<> localEnv <> prevEnv) $ evalCtx (Atom <$> args) (outF <$> vs) (outF body)
  modify (const globalEnv)
  pure v
applyLambda _ _ = error "Lib.AST.applyLambda: unreachable code (not lambda)"

-- Helpers {{{
-- applyCtx :: [ASTF AST'] -> [ASTF AST'] -> ASTF AST' -> Interp AST'
-- applyCtx vars defs v = local (<> newEnv vars defs) (alg v)

evalCtx :: [ASTF AST'] -> [ASTF AST'] -> ASTF AST' -> Interp AST'
evalCtx vars defs v = do
  -- oldestEnv <- get
  modify (<> newEnv vars defs)
  (oldEnv, prevEnv) <- liftA2 (,) get ask
  liftIO $ print prevEnv
  liftIO $ print oldEnv
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

newEnv :: [ASTF a] -> [ASTF AST'] -> Env
newEnv vars = packEnv . zipWith ((,) . getAtom) vars
  where packEnv = Env . fmap Fix . M.fromList

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
  ]
--   [ ("+",   varOp (int 0) (wrap int (+) getInt))
--   , ("-",   varOp (int 0) (wrap int (-) getInt))
--   , ("*",   varOp (int 1) (wrap int (*) getInt))
--   , ("/",   binOp (wrap int div getInt))
--   , ("div", binOp (wrap int div getInt))
--   , ("mod", binOp (wrap int mod getInt))
--   -- , ("quotient",  binOp quot)
--   -- , ("remainder", binOp rem)
--   , ("=",  boolOp2 getInt (==))
--   , ("<",  boolOp2 getInt (<))
--   , ("eq?", binOp eqAll)
--   -- , ("string=?",  boolOp2 getStr (==))
--   -- , ("string<?",  boolOp2 getStr (<))
--   -- , ("string>?",  boolOp2 getStr (>))
--   -- , ("string<=?", boolOp2 getStr (<=))
--   -- , ("string>=?", boolOp2 getStr (>=))

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
isAtom [Fix(Atom _)] = pure $ bool True
isAtom _             = pure $ bool False
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
