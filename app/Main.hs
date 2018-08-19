module Main where

import System.Process.Corecursive

main :: IO ()
main = corecursiveMain parse unparse go
  where
    parse :: [String] -> IO [String]
    parse = pure . id

    unparse :: [String] -> IO [String]
    unparse = pure  . id

    go self args
      | length args > 100  = print "done"
      | otherwise          = print args >> callback self (show (length args):args)
