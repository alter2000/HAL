module Main where

import Control.Exception ( handle )
import System.Environment ( getArgs )

import Lib.AST ( primEnv )
import Util ( halt, interpret, repl )

main :: IO ()
main = handle halt $ getArgs >>= \allArgs -> case allArgs of
  [] -> repl primEnv
  ["-i"] -> repl primEnv
  as | "-i" `elem` as && "-i" `isLast` as ->
    (dropI as >>= interpret primEnv) >>= repl . snd
  as -> dropI as >>= interpret primEnv >>= print . fst

isLast :: Eq a => a -> [a] -> Bool
isLast _ [] = False
isLast p [x] = p == x
isLast p (_:xs) = isLast p xs

dropI :: [String] -> IO [String]
dropI = pure . filter (/= "-i")
