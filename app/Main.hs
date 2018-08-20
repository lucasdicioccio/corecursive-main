module Main where

import Control.Concurrent.Async
import qualified Data.Text as Text
import System.Process.Corecursive.Base (Self(..))
import System.Process.Corecursive
import System.Posix.Process

data Arg = 
    Help
  | Things [String]
  | SSHLeader String [String] -- User [Host]

main :: IO ()
main = runCorecursiveApp (app parse unparse go)
  where
    parse :: [String] -> IO Arg
    parse []                  = pure Help
    parse ("rec":xs)          = pure $ Things xs
    parse ("ssh-leader":x:xs) = pure $ SSHLeader x xs
    parse args                =
        putStrLn ("un-understood arguments: " ++ show args) >> pure Help

    unparse :: Arg -> IO [String]
    unparse Help        = pure []
    unparse (Things xs) = pure ("rec":xs)

    go _ Help                    = printHelp
    go self arg@(Things xs)      = goThings self xs
    go self arg@(SSHLeader u hs) = goSSHLeader self u hs

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

printHelp = putStrLn $ unlines [
    "usage: "
  , "<exe> rec   # print ten recursive things"
  , "<exe> ssh-leader <user> <hostname|..> # copy as user on hosts and call 'rec' on each of them concurrently"
  ]
