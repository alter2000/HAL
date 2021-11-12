module Main where

-- import Control.Monad
import Control.Exception as E

-- import Data.Functor
-- import Data.Foldable
-- import Data.Traversable
import System.Console.Haskeline
import System.Environment ( getArgs )
-- import System.IO

import Util
-- import Types.Interp
-- import Types.AST

main :: IO ()
main = E.handle except $ getArgs >>= \allArgsEver -> case allArgsEver of
  [] -> repl $ Right defaultBehavior
  ["-i"] -> repl $ Right defaultBehavior
  as | "-i" `elem` as && "-i" `isLast` as -> do
    putStrLn "read all files, then drop into repl"
  _as' ->
    -- let as = filter (=="-i") as'
    putStrLn $ "read all files in loop, feeding previous arguments into"
            <> " next computation"
    -- [String] -> (String -> AST) -> [AST]
    -- acc :: AST -> Env -> Env
    -- runInterp $ forM_ as $ \f -> withFile f ReadMode $ readFile >>= interpret primEnv

isLast :: Eq a => a -> [a] -> Bool
isLast _ [] = False
isLast p [x] = p == x
isLast p (_:xs) = isLast p xs
