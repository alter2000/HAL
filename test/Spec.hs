module Main
  -- ( 
  -- )
  where


import Test.Hspec
import Test.Tasty ( defaultMain )
import Data.Map as M
import Data.Either

import RecursionSchemes
import Util
import Parser.AST
import Types.AST
import Lib.AST

import Golden

stdEnv :: IO Env
stdEnv = snd <$> interpretFile primEnv "./stdlib.scm"

getExpr :: Map VarName AST' -> String -> IO (AST', Env)
getExpr e = flip runStep (Env e) . applyRewriteRules . fromRight (error "failed parse") . parse

main :: IO ()
main = do
  (Env env) <- stdEnv
  putStrLn "Starting Unit Tests"
  hspec (utests env)
  putStrLn "Starting Golden Tests"
  defaultMain tests

utests :: Map VarName AST' -> SpecWith ()
utests env = describe "HAL" $ do

    it "(cons 1 2)" $ do
        (ast, _env') <- getExpr env "(cons 1 2)"
        ast `shouldBe` dlist [int 1] (int 2)

    it "(cons 1 (cons 2 (cons 3 '())))" $ do
        (ast, _env') <- getExpr env "(cons 1 (cons 2 (cons 3 '())))"
        ast `shouldBe` list [int 1, int 2, int 3]

    it "(car (cons 1 2))" $ do
        (ast, _env') <- getExpr env "(car (cons 1 2))"
        ast `shouldBe` int 1

    it "(cdr (cons 1 2))" $ do
        (ast, _env') <- getExpr env "(cdr (cons 1 2))"
        ast `shouldBe` int 2

    it "(cdr '(1 2 3))" $ do
        (ast, _env') <- getExpr env "(cdr '(1 2 3))"
        ast `shouldBe` list [int 2, int 3]

    it "(eq? 1 1)" $ do
        (ast, _env') <- getExpr env "(eq? 1 1)"
        ast `shouldBe` bool True

    it "(eq? (+ 1 1) 2)" $ do
        (ast, _env') <- getExpr env "(eq? (+ 1 1) 2)"
        ast `shouldBe` bool True

    it "(eq? 'foo (car '(foo bar)))" $ do
        (ast, _env') <- getExpr env "(eq? 'foo (car '(foo bar)))"
        ast `shouldBe` bool True

    it "(eq? 'foo 'bar)" $ do
        (ast, _env') <- getExpr env "(eq? 'foo 'bar)"
        ast `shouldBe` bool False

    it "(eq? '() '())" $ do
        (ast, _env') <- getExpr env "(eq? '() '())"
        ast `shouldBe` bool True

    it "(atom? 'foo)" $ do
        (ast, _env') <- getExpr env "(atom? 'foo)"
        ast `shouldBe` bool True

    it "(atom? '(1 2 3))" $ do
        (ast, _env') <- getExpr env "(atom? '(1 2 3))"
        ast `shouldBe` bool False

    it "(atom? '())" $ do
        (ast, _env') <- getExpr env "(atom? '())"
        ast `shouldBe` bool True

    it "(div (* 5 2) (- 3))" $ do
        (ast, _env') <- getExpr env "(div (* 5 2) (- 3))"
        ast `shouldBe` int (-4)

    it "(< (* 2 2) 5)" $ do
        (ast, _env') <- getExpr env "(< (* 2 2) 5)"
        ast `shouldBe` bool True

    it "(mod (+ 5 5) 3)" $ do
        (ast, _env') <- getExpr env "(mod (+ 5 5) 3)"
        ast `shouldBe` int 1

    it "(quote pog)" $ do
        (ast, _env') <- getExpr env "(quote pog)"
        ast `shouldBe` atom "pog"

    it "'toto" $ do
        (ast, _env') <- getExpr env "'toto"
        ast `shouldBe` atom "toto"

    it "(quote (+ 1 2))" $ do
        (ast, _env') <- getExpr env "(quote (+ 1 2))"
        ast `shouldBe` list [atom "+", int 1, int 2]

    it "'(+ 1 2)" $ do
        (ast, _env') <- getExpr env "'(+ 1 2)"
        ast `shouldBe` list [atom "+", int 1, int 2]

    it "(lambda (a b) (+ a b))" $ do
        (Fix ast, _env') <- getExpr env "(lambda (a b) (+ a b))"
        ast `shouldBe` Lambda ["a","b"] (list [atom "+", atom "a", atom "b"]) (Env env)

    it "((lambda (a b) (+ a b)) 1 2)" $ do
        (ast, _env') <- getExpr env "((lambda (a b) (+ a b)) 1 2)"
        ast `shouldBe` int 3

    -- it "foo" $ do
    --     (ast, _env') <- getExpr env "foo"
    --     ast `shouldBe` ("error: Symbol foo is not bound" :: String)

    it "(define foo 42)" $ do
        (ast, _env') <- getExpr env "(define foo 42)"
        ast `shouldBe` atom "foo"

    it "foo" $ do
        (_ast, env') <- getExpr env "(define foo 42)"
        (ast', _env'') <- getExpr (getEnv env') "foo"
        ast' `shouldBe` int 42

    it " (define add (lambda (a b) (+ a b)))" $ do
        (ast, _env') <- getExpr env "(define add (lambda (a b) (+ a b)))"
        ast `shouldBe` atom "add"

    it "(add 1 3)" $ do
        (_ast, env') <- getExpr env "(define add (lambda (a b) (+ a b)))"
        (ast', _env'') <- getExpr (getEnv env') "(add 1 3)"
        ast' `shouldBe` int 4

    it "(define (sub a b) (- a b))" $ do
        (ast, _env') <- getExpr env "(define (sub a b) (- a b))"
        ast `shouldBe` atom "sub"

    it " (let ((a 2) (b (+ 1 2))) (+ a b))" $ do
        (ast, _env') <- getExpr env " (let ((a 2) (b (+ 1 2))) (+ a b))"
        ast `shouldBe` int 5

    it "(cond (#f 1) (#t (+ 1 1)))" $ do
        (ast, _env') <- getExpr env "(cond (#f 1) (#t (+ 1 1)))"
        ast `shouldBe` int 2

    it "(cond ((eq? 'foo (car '(foo bar))) 'here) ((eq? 1 2) 'there) (#t 'nope))" $ do
        (ast, _env') <- getExpr env "(cond ((eq? 'foo (car '(foo bar))) 'here) ((eq? 1 2) 'there) (#t 'nope))"
        ast `shouldBe` atom "here"

    it "(fact 10)" $ do
        (ast, _env') <- getExpr env "(fact 10)"
        ast `shouldBe` int 3628800

    it "(fib 21)" $ do
        (ast, _env') <- getExpr env "(fib 21)"
        ast `shouldBe` int 10946

    it "(merge-sort)" $ do
        (ast, _env') <- getExpr env "(merge-sort '(39 16 22 24 17 29 18 26 27 3 34 25 10 6 7 12 8 30 2 21 13 36 14 38 32 41 40 4 35 19 5 33 23 9 15 31 28 20 42 37 11 1))"
        ast `shouldBe` list [int 1, int 2, int 3, int 4, int 5, int 6, int 7, int 8, int 9, int 10, int 11, int 12, int 13, int 14, int 15, int 16, int 17, int 18, int 19, int 20, int 21, int 22, int 23, int 24, int 25, int 26, int 27, int 28, int 29, int 30, int 31, int 32, int 33, int 34, int 35, int 36, int 37, int 38, int 39, int 40, int 41, int 42]
