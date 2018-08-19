-- | A module to facilitate and demonstrate a programming pattern where
-- application instances for a system is (co-)recursive.
module System.Process.Corecursive
    ( corecursiveMain
    , callback
    , app
    ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import System.Environment (getArgs, getExecutablePath)
import System.Process (callProcess)

import System.Process.Corecursive.Types

runCorecursiveApp :: MonadIO m => App m FilePath [String] arg -> m ()
runCorecursiveApp (App parse unparse go) =
    corecursiveMain parse unparse go

corecursiveMain
  :: MonadIO m
  => ([String] -> m arg)
  -> (arg -> m [String])
  -> (Self m FilePath [String] arg -> arg -> m ())
  -> m ()
corecursiveMain parse unparse go = do
    args <- liftIO $ getArgs
    a <- parse args
    self <- liftIO $ getExecutablePath
    go (Self (pure self) unparse) a

callback :: MonadIO m => Self m FilePath [String] arg -> arg -> m ()
callback self a = do
    exe <- executable self
    args <- unparse self $ a
    liftIO $ callProcess exe args
