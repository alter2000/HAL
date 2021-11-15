-- | HALP NEED EXPLAIN
--
-- * Single expression -> StateT (Interp)
--   * AST' -> Scope AST'
module Lib.AST
  where


import Debug.Trace
import Control.Monad.Except
import Control.Monad.Reader
import Control.Arrow
import qualified Data.Map as M

import Types.Exceptions ( HALError(..) )
import RecursionSchemes
import Types.Cofree

import Types.AST
import Types.Interp
import Types.Pos
-- import Lib.Env

type EvalAST = Cofree ASTF Pos

type Alg f a = f a -> a

-- | CV-algebra, can:
--
--  * see whole structure
--  * see all optimizations done
--  * cache intermediate results
cvAlg :: a
cvAlg = undefined

resolveScope :: Scope f -> Env -> IO (Either HALError f)
resolveScope s = runReaderT s >>> runExceptT

runStep :: AST' -> Env -> IO (Either HALError (AST', Env))
runStep = cata alg >>> runInterp >=> resolveScope

alg :: Alg ASTF (Interp AST')
alg  (Atom a) = pure $ atom a
alg   (Int a) = pure $ int a
alg  (Bool a) = pure $ bool a
-- alg  (Real a) = real a
alg   (Str a) = pure $ str a
alg  (List s) = list <$> sequence s
alg (DottedList as t) = sequence as >>= (<$> t) . dlist
alg (Function ps b c) = handleFunction ps b c

handleFunction :: [String] -> [Interp AST'] -> Env -> Interp AST'
handleFunction params body ctx = undefined

builtins :: M.Map VarName AST'
builtins = M.empty
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
--   , (">",  boolOp2 getInt (>))
--   , ("/=", boolOp2 getInt (/=))
--   , (">=", boolOp2 getInt (>=))
--   , ("<=", boolOp2 getInt (<=))
--   , ("&&", boolOp2 getBool (&&))
--   , ("||", boolOp2 getBool (||))
--   , ("eq?", binOp eqAll)
--   -- , ("string=?",  boolOp2 getStr (==))
--   -- , ("string<?",  boolOp2 getStr (<))
--   -- , ("string>?",  boolOp2 getStr (>))
--   -- , ("string<=?", boolOp2 getStr (<=))
--   -- , ("string>=?", boolOp2 getStr (>=))
--   , ("quote", quote)
--   , ("car", car)
--   , ("cdr", cdr)
--   , ("cons", cons)
--   , ("atom?", isAtom)
--   , ("cond", cond)
--   ]

-- primEnv :: Env
-- primEnv = Env $ builtin <$> builtins

quote :: [AST'] -> Interp AST'
-- quote [a] = func [] (Just "quote") [a] . Env $ builtin <$> builtins
quote as  = throwError $ BadArguments nPos (length as) 1

cond :: [AST'] -> Interp AST'
cond (Fix (List [Fix a, b]):rest) = case cvAlg a of
  Fix (Bool True) -> pure b
  Fix (Bool False) -> cond rest
  _ -> throwError $ TypeMismatch nPos "cond only handles bools"
cond _ = throwError $ TypeMismatch nPos "cond only handles bools"

-- TODO: really need to handle 'quote'?
car :: [AST'] -> Interp AST'
car (Fix    (List[Fix(Atom "quote"), Fix(List(r:_))]):_) = pure $ trace (show r) r
car (Fix a'@(List(Fix(Atom _):_)):_) = cvAlg $ List [atom "car", cvAlg a']
car (Fix    (List(a          :_)):_) = pure a
car _ = pure $ list []

cdr :: [AST'] -> Interp AST'
cdr (Fix    (List[Fix(Atom "quote"), Fix(List(_:r))]):_) = pure $ list r
cdr (Fix a'@(List(Fix(Atom _):_)):_) = cvAlg $ List [atom "cdr", cvAlg a']
cdr (Fix    (List(_          :r)):_) = pure $ list r
cdr _ = pure $ list []

cons :: [AST'] -> Interp AST'
cons [a, Fix (List bs)] = pure $ list $ a:bs
cons [a, b]             = pure $ list [a, b]
cons as = throwError $ BadArguments nPos 2 (length as)

isAtom :: [AST'] -> Interp AST'
isAtom [Fix(Atom _)] = pure $ bool True
isAtom _             = pure $ bool False

-- rewrite rules {{{
 -- [.] `(define (func p) expr)` -> `(define func (lambda (p) (expr)))`
 -- [ ] `(let ((a 1) (b 2)) (+ a b))` -> `((lambda (a b) (+ a b)) 1 2)`
applyRewriteRules :: AST' -> AST'
applyRewriteRules (Fix(List [
    Fix(Atom "define"), Fix(List (Fix(Atom fname) : args)), body]))
  = list [atom "define", atom fname, list [atom "lambda", list args, body]]
applyRewriteRules (Fix(List [Fix(Atom "let"), Fix(List pairs), body]))
  = list [list [atom "lambda", list $ fsts pairs, body], list $ snds pairs]
    where fsts = undefined
          snds = undefined
applyRewriteRules a = a
-- }}}

-- testing {{{

-- testFunc :: [Fix ASTF] -> Fix ASTF
-- testFunc b = Fix $ Function { params=["x","y"], args=Nothing
--                     , closure=Env $ Fix . Builtin <$> builtins
--                     , body=b }

testList :: Fix ASTF
-- testList = list [atom "eq?", list [atom "+", int 1, int 2], list [atom "quote", int 3]]
-- testList = list [atom "quote", list [atom "+", int 1, int 2]]
testList = list [atom "cond",
  list [list [atom "eq?", list [atom "quote", atom "foo"],
                             list [atom "car",   list [atom "quote", list [atom "foo", atom "bar"]]]],
        list [atom "quote", atom "here"]],
  list [list [atom "eq?", int 1, int 2], list [atom "quote", atom "there"]],
  list [bool True, list [atom "quote", atom "nope"]]
  ]

n2list :: Fix ASTF
n2list = list [atom "eq?", list [atom "quote", atom "foo"],
                           carlist]
carlist :: Fix ASTF
carlist = list [atom "car", qlist]

qlist :: Fix ASTF
qlist = list [atom "quote", list [atom "foo", atom "bar"]]

car2list :: Fix ASTF
car2list = list [atom "car", list [atom "quote", list [str "a", str "b", str "c"]]]
-- }}}
