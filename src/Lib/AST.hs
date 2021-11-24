-- | HALP NEED EXPLAIN
--
-- * Single expression -> StateT (Interp)
--   * AST' -> Scope AST'
module Lib.AST
  where


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

-- | CV-algebra, can:
--
--  * see whole structure
--  * see all optimizations done
--  * cache intermediate results
cvAlg :: ASTPos -> Interp ASTPos
cvAlg = undefined

evalTerm :: AST' -> Env -> InputT IO (AST', Env)
evalTerm ast e = liftIO $ handle (halExcept >>> (>> pure (list [], e)))
  (runInterp (applyRewriteRules ast) >>= resolveScope $ e)

runStep :: AST' -> Env -> IO (AST', Env)
runStep = cataM alg >>> runInterp >=> resolveScope


-- | needs more flesh, usable while inside 'Control.Monad.Except.ExceptT'
halExcept :: HALError -> IO ()
halExcept = hPutStrLn stderr . displayException

alg :: ASTF AST' -> Interp AST'
alg  (Int a) = pure $ int a
alg (Bool a) = pure $ bool a
alg  (Str a) = pure $ str a
alg (DottedList as t) = pure $ dlist as t
alg (Lambda ps b c) = pure $ func ps b c
alg (Builtin b) = pure $ mkBuiltin b
alg (List []) = pure $ list []
alg (List as) = handleList (outF <$> as)
                          -- TODO: DANGEROUS? ↓ pure $ atom a
alg (Atom a) = maybe (trace ("atom? " <> a) $ throw $ UndefinedSymbol nPos a)
  pure . M.lookup a . getEnv =<< get
-- alg (Real a) = real a

handleList :: [ASTF AST'] -> Interp AST'
handleList [Atom "quote", a] = trace "list quote" $ pure $ Fix a
handleList (Lambda{}:_) = throw $ TypeMismatch nPos "procedure"
handleList [Atom "define", var, def] = -- get >>= \(Env _) ->
  retAtom var >> evalCtx [var] [def] var >> pure (Fix var)
handleList a = pure $ list $ Fix <$> a

-- Helpers {{{
retAtom :: ASTF a -> Interp (ASTF a)
retAtom (Atom a) = pure $ Atom a
retAtom _ = throw $ TypeMismatch nPos "expected atom"

getAtom :: ASTF a -> VarName
getAtom (Atom a) = a
getAtom _ = throw $ TypeMismatch nPos "expected atom"

applyCtx :: [ASTF AST'] -> [ASTF AST'] -> ASTF AST' -> Interp AST'
applyCtx vars defs v =  ask >>= \(Env e) ->
  local (const . Env $ (newEnv vars defs <> e)) $ alg v

evalCtx :: [ASTF AST'] -> [ASTF AST'] -> ASTF AST' -> Interp AST'
evalCtx vars defs v = get >>= \(Env e) ->
  put (Env $ newEnv vars defs <> e) >> alg v

newEnv :: [ASTF a] -> [f (Fix f)] -> M.Map VarName (Fix f)
newEnv vars defs = fmap Fix $ M.fromList $ zipWith ((,) . getAtom) vars defs

getLambda :: VarName -> [AST'] -> Interp AST'
getLambda l as = maybe (trace "lambda?" $ throw $ UndefinedSymbol nPos l)
  (`applyLambda` as) . M.lookup l . getEnv =<< get

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

primEnv :: Env
primEnv = Env $ mkBuiltin <$> builtins

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
-- }}}

-- rewrite rules {{{
 -- [.] `(define (func p) expr)` -> `(define func (lambda (p) (expr)))`
 -- [ ] `(let ((a 1) (b 2)) (+ a b))` -> `((lambda (a b) (+ a b)) 1 2)`
applyRewriteRules :: AST' -> Interp AST'
applyRewriteRules = rewrite . outF

rewrite :: ASTF AST' -> Interp AST'
rewrite (List [Fix(Atom "define"), Fix(List (Fix(Atom fname) : args)), body])
  = pure $ list [atom "define", atom fname,
                  list [atom "lambda", list args, body]]
rewrite (List [Fix(Atom "let"), Fix(List pairs), body])
  = pure $ list [list [atom "lambda", list $ fsts pairs, body],
                  list $ snds pairs]
rewrite (List [Fix(List[Fix(Atom "lambda"), Fix(List args), Fix(List bd)])])
  = gets $ Fix . Lambda (getAtom . outF <$> args) bd
rewrite (List (Fix(Atom a):as)) = pure $ list $ maybe (atom a:as)
  ((: as) . mkBuiltin) $ M.lookup a builtins
rewrite a = pure $ Fix a

fsts :: [a] -> [a]
fsts [] = []
fsts (x:xs) = x : snds xs
snds :: [a] -> [a]
snds [] = []
snds (_:xs) = fsts xs

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
