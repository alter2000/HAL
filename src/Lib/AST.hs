-- | HALP NEED EXPLAIN
--
-- * Single expression -> StateT (Interp)
--   * AST' -> Scope AST'
module Lib.AST
  where


import Debug.Trace

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Arrow
import qualified Data.Map as M

import Control.Exception
import System.IO
import System.Console.Haskeline

import Types.Exceptions
import RecursionSchemes
import Types.Cofree

import Types.AST
import Types.Interp
import Types.Pos

type ASTPos = Cofree ASTF Pos

type Alg f a = f a -> a

evalTerm :: AST' -> Env -> InputT IO (AST', Env)
evalTerm ast e = liftIO $ handle (halExcept >>> (>> pure (list [], e)))
  (runStep (applyRewriteRules ast) e)

runStep :: AST' -> Env -> IO (AST', Env)
runStep = cataMCps alg >>> runInterp >=> resolveScope

-- eval :: AST' -> Interp AST'
-- eval a = get >>= (runStep a (flip fmap) \r ->
--             case r of
--               Left he -> liftIO (halExcept he) >> pure $ list []
--               Right (a', e') -> put e' >> pure a')

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
alg (Atom a) = maybe (checkBuiltin a)
  pure . M.lookup a . getEnv =<< get

alg (List [Fix(Atom "quote"), a]) = pure a
alg (List [Fix(Atom "define"), Fix var, Fix def]) =
  retAtom var >> evalCtx [var] [def] var >> pure (Fix var)
alg (List [Fix(Atom "lambda"), Fix(List ps), b]) =
  asks $ func (getAtom . outF <$> ps) b
alg (List (Fix(Lambda vs (Fix body) env): ps)) = ask >>= \gEnv ->
  modify (const $ gEnv <> env) >> get >>= \e' ->
  trace ("env: " <> show e') $ applyCtx (Atom <$> vs) (outF <$> ps) body
alg (List (Fix(Builtin (Func a)):as)) = a as
alg (List []) = pure $ list []
alg a = pure $ Fix a

-- handleList ((Atom a):as) = pure $ atom a -- applyLambda a $ (runStep <*> get) <$> Fix <$> as
-- handleList a@(List{}:_) = pure $ list $ Fix <$> a
-- handleList (Fix a@List{}:as) = get >>= liftIO . runStep (Fix a) >>=
--   either throw (\(r, env) -> put env >> pure r)

checkBuiltin :: VarName -> Interp AST'
checkBuiltin v = get >>= \(Env env) -> case M.lookup v env of
  Just a -> pure a
  Nothing | v `elem` specialForms -> pure $ atom v
          | otherwise -> throw $ UndefinedSymbol nPos v
    where specialForms = ["define", "lambda"]

-- Helpers {{{
applyCtx :: [ASTF AST'] -> [ASTF AST'] -> ASTF AST' -> Interp AST'
applyCtx vars defs v = trace ("var: " <> show v) $ local (newEnv vars defs <>) (alg v)

evalCtx :: [ASTF AST'] -> [ASTF AST'] -> ASTF AST' -> Interp AST'
evalCtx vars defs v = modify (newEnv vars defs <>) >> alg v

retAtom :: ASTF a -> Interp (ASTF a)
retAtom (Atom a) = pure $ Atom a
retAtom _ = throw $ TypeMismatch nPos "expected atom"

getAtom :: ASTF a -> VarName
getAtom (Atom a) = a
getAtom _ = throw $ TypeMismatch nPos "expected atom"

newEnv :: [ASTF a] -> [ASTF AST'] -> Env
newEnv vars = Env . fmap Fix . M.fromList . zipWith ((,) . getAtom) vars

-- getLambda :: VarName -> [AST'] -> Interp AST'
-- getLambda l as = maybe (trace "lambda?" $ throw $ UndefinedSymbol nPos l)
--   (`applyLambda` as) . M.lookup l . getEnv =<< get

applyLambda :: AST' -> [AST'] -> Interp AST'
applyLambda = undefined -- ask
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
  = list $ [list [atom "lambda", list $ fsts pairs, body]] <> snds pairs
-- rewrite (List [Fix(List[Fix(Atom "lambda"), Fix(List args), Fix(List bd)])])
--   = gets $ Fix . Lambda (getAtom . outF <$> args) bd
rewrite (List (Fix(Atom a):as)) = list $ maybe (atom a:as)
  ((: as) . mkBuiltin) $ M.lookup a builtins
rewrite a = Fix a

unzipList :: [AST'] -> [(AST', AST')]
unzipList [] = []
unzipList (Fix(List [a, b]):xs) = (a, b) : unzipList xs
unzipList (_:xs) = unzipList xs

fsts :: [AST'] -> [AST']
fsts = fmap fst . unzipList

snds :: [AST'] -> [AST']
snds = fmap snd . unzipList
-- }}}
