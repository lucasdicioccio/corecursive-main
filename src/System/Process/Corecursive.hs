-- | A module to facilitate and demonstrate a programming pattern where
-- application instances for a system is (co-)recursive.
module System.Process.Corecursive
    ( runCorecursiveApp
    , callback
    , readCallback
    -- * re-exported for convenience
    , App
    , Self
    , app
    ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import System.Environment (getArgs, getExecutablePath)
import System.Process (callProcess, readProcess)

import System.Process.Corecursive.Types

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
