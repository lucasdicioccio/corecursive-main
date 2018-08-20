{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A module to facilitate and demonstrate a programming pattern where
-- application instances for a system is (co-)recursive.
module System.Process.Corecursive
    ( runCorecursiveApp
    , callback
    , readCallback
    -- * over SSH
    , SSHInfo(..)
    , UserName(..)
    , HostName(..)
    , sshCallback
    -- * re-exported for convenience
    , App
    , Self
    , app
    ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Environment (getArgs, getExecutablePath)
import System.Process (callProcess, readProcess)

import System.Process.Corecursive.Base (runApp, app, App, Self(..))

-- | Run function for an App which arguments are read from the command line
-- using 'getArgs' and for which the binary is discovered from
-- 'getExecutablePath'.
runCorecursiveApp :: MonadIO m => App m [String] arg FilePath () -> m ()
runCorecursiveApp =
    runApp (liftIO getExecutablePath) (liftIO getArgs)

-- | Callback running the Self executable locally and waiting until completion
-- using 'callProcess'.
callback :: MonadIO m => Self m [String] arg FilePath -> arg -> m ()
callback self a = do
    exe <- executable self
    args <- unparse self $ a
    liftIO $ callProcess exe args

-- | Callback running the Self executable locally and waiting until completion
-- using 'readProcess'.
readCallback :: MonadIO m => Self m [String] arg FilePath -> arg -> m String
readCallback self a = do
    exe <- executable self
    args <- unparse self $ a
    liftIO $ readProcess exe args ""

newtype UserName = UserName { getUserName :: Text }
  deriving (Eq,Ord,Show,IsString)

newtype HostName = HostName { getHostName :: Text }
  deriving (Eq,Ord,Show,IsString)

data SSHInfo = SSHInfo {
    user       :: UserName
  , host       :: HostName
  , remotePath :: FilePath
  } 

sshCallback
  :: MonadIO m
  => Self m [String] arg FilePath
  -> SSHInfo
  -> arg
  -> m String
sshCallback self ssh a = do
    exe <- executable self
    args <- unparse self $ a
    copySSH ssh exe
    runSSH ssh args
  where
    copySSH (SSHInfo user host rpath) lpath =
        let host' = Text.unpack (getHostName host) in
        let user' = Text.unpack (getUserName user) in
        liftIO $ callProcess "scp" [ lpath , user' ++ "@" ++ host' ++ ":" ++ rpath]
    runSSH (SSHInfo user host rpath) args =
        let host' = Text.unpack (getHostName host) in
        let user' = Text.unpack (getUserName user) in
        liftIO $ readProcess "ssh" (["-l", user', host', "--", rpath] ++ args) ""
