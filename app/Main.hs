{-# LANGUAGE StaticPointers   #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Concurrent.Async
import Control.Distributed.Closure (closure, unclosure)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Text as Text
import System.Process.Corecursive.Base (Self(..))
import System.Process.Corecursive.Closure
import System.Process.Corecursive
import System.Posix.Process

data Arg =
    Help
  | Things [String]
  | Job Int
  | SSHLeader String [String] -- User [Host]
  | StartB64
  | Base64Closure ByteString.ByteString

main :: IO ()
main = runCorecursiveApp (app parse unparse go)
  where
    parse :: [String] -> IO Arg
    parse []                  = pure Help
    parse ("rec":xs)          = pure $ Things xs
    parse ("ssh-leader":x:xs) = pure $ SSHLeader x xs
    parse ("job":x:[])        = pure $ Job (read x)
    parse ("start-b64":[])    = pure StartB64
    parse ("b64":x:[])        = pure $ Base64Closure (ByteString.pack x)
    parse args                =
        putStrLn ("un-understood arguments: " ++ show args) >> pure Help

    unparse :: Arg -> IO [String]
    unparse (Things xs)         = pure ("rec":xs)
    unparse (Job n)             = pure ("job":show n:[])
    unparse (Base64Closure arg) = pure ("b64":ByteString.unpack arg:[])

    go _ Help                    = printHelp
    go self arg@(Things xs)      = goThings self xs
    go self arg@(SSHLeader u hs) = goSSHLeader self u hs
    go self arg@(Job n)          = goBigJob self n
    go self StartB64             = goStartB64 self
    go self (Base64Closure arg)  = goClosure self (unclosure $ parseClosureB64 arg)

goBigJob self n
    -- If n is large, split the input in two and ask two jobs to work on
    -- smaller inputs each job may do so recursively. This is possible because
    -- multiplication distributes over multiplication.
    | n > 1000   = do
        let (j1, j2) = splitBigJob n
        [r1, r2] <- mapConcurrently (\n -> readCallback self (Job n)) [j1, j2]
        putStrLn (show $ read @Int r1 + read @Int r2)
    | otherwise = putStrLn (show $ n * 2)
  where
    splitBigJob n = let k = n `div` 2 in (k, n - k)

goSSHLeader self user hosts = do
    -- Prepare to copy in homedirs with an index so that each copy gets its own
    -- number (e.g., if two hosts resolve to the same machine).
    let rpath = "~/corecursive-main-exe"
    let sshinfos = [ SSHInfo (UserName $ Text.pack user) (HostName $ Text.pack host) (rpath ++ "." ++ idx) | (idx,host) <- zip (fmap show [1..]) hosts ]

    -- Use Async's 'mapConcurrently' (best thing since sliced bread) to
    -- run an SSH session for each call and collect the results.
    results <- mapConcurrently callBackThing sshinfos

    -- Collect per-SSH session result.
    putStrLn $ unlines (zipWith (\h r -> h ++ ":" ++ r) hosts results)
  where
    -- Construct a callback using a constructor for Arg.
    callBackThing sshinfo = sshCallback self sshinfo (Things ["ssh-ed"])

goThings self xs
  -- Stop recursively calling oneself after some value (each time we fork a
  -- process, ten is not that small).
  | length xs > 10 = print "done"
  -- Print to stdout, appending the result of the (longer) co-recursive call.
  | otherwise      = do
      pid <- getProcessID
      dat <- readCallback self (Things $ show pid : xs)
      putStr $ show pid ++ show xs ++ " " ++ dat

goStartB64 self = do
    let go = readCallback self . Base64Closure . unparseClosureB64
    putStrLn =<< go (static closure1)
    putStrLn =<< go (static closure2)

goClosure self action = do
    print "from-closure"
    action

closure1 = print "hello"
closure2 = print "world"

printHelp = putStrLn $ unlines [
    "usage: "
  , "<exe> rec                             # print ten recursive things"
  , "<exe> ssh-leader <user> <hostname|..> # copy as user on hosts and call 'rec' on each of them concurrently"
  , "<exe> job <n>                         # double a number by recursively breaking it in small pieces"
  , "<exe> start-b64                       # demo using a closure serialization"
  ]
