module TestObjects
  -- ( 
  -- )
  where

import Lib.AST
import Types.AST
import RecursionSchemes

testFunc :: AST' -> AST'
testFunc b = Fix $ Lambda { fnArgs=["x","y"] , fnEnv=primEnv, fnBody=b }

testEq, testQuote, testList :: AST'
testEq = list [atom "eq?", list [atom "+", int 1, int 2], list [atom "quote", int 3]]
testQuote = list [atom "quote", list [atom "+", int 1, int 2]]
testList = list [atom "cond",
  list [list [atom "eq?", list [atom "quote", atom "foo"],
                             list [atom "car",   list [atom "quote", list [atom "foo", atom "bar"]]]],
        list [atom "quote", atom "here"]],
  list [list [atom "eq?", int 1, int 2], list [atom "quote", atom "there"]],
  list [bool True, list [atom "quote", atom "nope"]]
  ]

testlambda :: AST'
testlambda = list [list [atom "lambda", list [atom "a", atom "b"]
                        ,atom "a"]
                  ,str "poggers", str "three"
                  ]

testlet :: AST'
testlet = list [atom "let", list [ list [atom "a", int 1]
                                 , list [atom "b", list [atom "+", int 1, int 2]]
                                 , list [atom "c", int 5]]
               ,list [atom "+", atom "a", atom "b", atom "c"]]

testvardefine :: AST'
testvardefine = list [atom "define", atom "poggers", int 1234]

testlambdadefine :: AST'
testlambdadefine = list [atom "define", list [atom "app", atom "arg1",
                                              atom "arg2", atom "arg3"]
                        ,list [atom "arg1", atom "arg2", atom "arg3"]]

n2list :: Fix ASTF
n2list = list [atom "eq?", list [atom "quote", atom "foo"],
                           carlist]
carlist :: Fix ASTF
carlist = list [atom "car", qlist]

qlist :: Fix ASTF
qlist = list [atom "quote", list [atom "foo", atom "bar"]]

car2list :: Fix ASTF
car2list = list [atom "car", list [atom "quote", list [str "a", str "b", str "c"]]]
