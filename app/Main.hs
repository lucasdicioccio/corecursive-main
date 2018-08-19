module Main where

import System.Process.Corecursive
import System.Posix.Process

main :: IO ()
main = runCorecursiveApp (app parse unparse go)
  where
    parse :: [String] -> IO [String]
    parse = pure . id

    unparse :: [String] -> IO [String]
    unparse = pure  . id

    go self args
      | length args > 10 = print "done"
      | otherwise        = do
          pid <- getProcessID
          dat <- readCallback self (show pid:args)
          putStr $ show pid ++ show args ++ " " ++ dat
