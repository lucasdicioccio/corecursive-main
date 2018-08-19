module Main where

import System.Process.Corecursive
import System.Posix.Process

newtype Arg = Arg { getListOfThings :: [String] }

numberOfThings :: Arg -> Int
numberOfThings = length . getListOfThings

append :: Show a => a -> Arg -> Arg
append x (Arg xs) = Arg (show x:xs)

main :: IO ()
main = runCorecursiveApp (app parse unparse go)
  where
    parse :: [String] -> IO Arg
    parse = pure . Arg

    unparse :: Arg -> IO [String]
    unparse = pure  . getListOfThings

    go self arg
      | numberOfThings arg > 10 = print "done"
      | otherwise               = do
          pid <- getProcessID
          dat <- readCallback self (append pid arg)
          putStr $ show pid ++ show (getListOfThings arg) ++ " " ++ dat
