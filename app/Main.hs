module Main where

import Control.Exception as E
import System.Environment ( getArgs )

import Util

main :: IO ()
main = handle except $ getArgs >>= \allArgs -> case allArgs of
  [] -> repl
  ["-i"] -> repl
  as | "-i" `elem` as && "-i" `isLast` as -> do
    putStrLn "read all files, then drop into repl"
  as -> pure (filter (/= "-i") as) >>
    putStrLn $ "read all files in loop, feeding previous arguments into"
            <> " next computation"
    -- [String] -> (String -> AST) -> [AST]
    -- acc :: AST -> Env -> Env
    -- runInterp $ forM_ as $ \f -> withFile f ReadMode $ readFile >>= interpret primEnv

isLast :: Eq a => a -> [a] -> Bool
isLast _ [] = False
isLast p [x] = p == x
isLast p (_:xs) = isLast p xs
