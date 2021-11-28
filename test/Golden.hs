module Golden
  ( tests
  )
  where


import Test.Tasty ( testGroup, TestName, TestTree )
import Test.Tasty.Golden ( goldenVsString )
import Data.Functor ((<&>))
import Data.Map
import Data.Either
import Data.ByteString.Lazy.Char8 hiding (readFile)

import Lib.AST
import Types.AST
import Parser.AST
import Util


tests :: TestTree
tests = testGroup "Golden tests"
  [ tastyGoldenRun "add"                "test/golden-in/add.scm"              "test/golden-out/add.txt"
  , tastyGoldenRun "if/then"            "test/golden-in/if_alt.scm"           "test/golden-out/if_alt.txt"
  , tastyGoldenRun "let"                "test/golden-in/let.scm"              "test/golden-out/let.txt"
  , tastyGoldenRun "lambda let"         "test/golden-in/test_let.scm"         "test/golden-out/test_let.txt"
  , tastyGoldenRun "eval bool"          "test/golden-in/eval_boolean.scm"     "test/golden-out/eval_boolean.txt"
  , tastyGoldenRun "eval bool ops"      "test/golden-in/eval_boolean_ops.scm" "test/golden-out/eval_boolean_ops.txt"
  , tastyGoldenRun "eval lambda"        "test/golden-in/eval_lambda.scm"      "test/golden-out/eval_lambda.txt"
  , tastyGoldenRun "quote"              "test/golden-in/test_quote.scm"       "test/golden-out/test_quote.txt"
  , tastyGoldenRun "car"                "test/golden-in/test_car.scm"         "test/golden-out/test_car.txt"
  , tastyGoldenRun "cdr"                "test/golden-in/test_cdr.scm"         "test/golden-out/test_cdr.txt"
  , tastyGoldenRun "cadadr"             "test/golden-in/test_cadadr.scm"      "test/golden-out/test_cadadr.txt"
  , tastyGoldenRun "greater than"       "test/golden-in/test_gt.scm"          "test/golden-out/test_gt.txt"
  , tastyGoldenRun "lexical scope 1"    "test/golden-in/test_scope1.scm"      "test/golden-out/test_scope1.txt"
  , tastyGoldenRun "lexical scope 2"    "test/golden-in/test_scope2.scm"      "test/golden-out/test_scope2.txt"
  , tastyGoldenRun "lexical scope 3"    "test/golden-in/test_scope3.scm"      "test/golden-out/test_scope3.txt"
  , tastyGoldenRun "Recursion 1"        "test/golden-in/test_fix.scm"         "test/golden-out/test_fix.txt"
  , tastyGoldenRun "Recursion 2"        "test/golden-in/test_fix2.scm"        "test/golden-out/test_fix2.txt"
  , tastyGoldenRun "fn args"            "test/golden-in/test_args.scm"        "test/golden-out/test_args.txt"
  , tastyGoldenRun "fold"               "test/golden-in/test_fold.scm"        "test/golden-out/test_fold.txt"
  ]

tastyGoldenRun :: TestName -> String -> FilePath -> TestTree
tastyGoldenRun name file gold = goldenVsString name gold $ evalTest file <&> pack . show

evalTest :: String -> IO AST'
evalTest file = do
  (Env env) <- stdEnv
  f <- readFile file
  fst <$> getExpr env f

getExpr :: Map VarName AST' -> String -> IO (AST', Env)
getExpr e = flip runStep (Env e) . applyRewriteRules . fromRight (error "failed parse") . parse

stdEnv :: IO Env
stdEnv = snd <$> interpretFile primEnv "./stdlib.scm"
