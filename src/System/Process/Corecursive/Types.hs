
-- | Types for building (co-)recursive system programs.
--
-- The tools in this module allow to define the call-site and the main entry
-- point of a (co-)recursive program.
--
-- However, this module does not define 'callback' usages so that implementers
-- are free to chose the mechanism they want to:
-- - start the recursive program
-- - retrieve a return-value (if the recursive program ever returns)
module System.Process.Corecursive.Types
    ( App (..)
    , app
    , runApp
    , Self (..)
    ) where

-- | A datatype representing an instance of the (co-)recursive program.
--
-- A reason why the records in this type are separate from 'App' is that a
-- calling program may need to execute a fair amount of progress
-- before knowing the right 'executable' (e.g., in the case of a remote
-- invocation). Also, sometimes one may want to adapt 'Self'.
data Self t msg arg inst = Self {
    executable :: t inst
  -- ^ An  action to retrieve an instance of the (co-)recursive program
  -- the 'inst' type is leaved as an argument to allow representing cases where
  -- the execution is actually remote.
  , unparse    :: arg -> t msg
  -- ^ An action to unparse arguments. Typically from 'unparseArgs'.
  }
instance Functor t => Functor (Self t msg arg) where
    fmap f (Self e u) = Self (fmap f e) u

-- | A datatype wrapping everything needed to make a (co-)recursive main
-- function.
--
-- Application authors may find this type useful because it has a Functor
-- instance, allowing to adapt the result of a computation.
--
-- See 'app'.
data App t msg arg inst ret = App {
    parseArgs          :: msg -> t arg
  -- ^ Function parsing argument from a serialized message.
  , unparseArgs        :: arg -> t msg
  -- ^ Function un-parsing an argument into a serialized message.
  , corecursiveProgram :: Self t msg arg inst -> arg -> t ret
  -- ^ Actual program to run from a specification.
  }

instance Functor t => Functor (App t msg arg inst) where
    fmap f (App p u cp) =
        App p u $ \self arg -> fmap f (cp self arg)

-- | Constructor for an App.
app
  :: (msg -> m arg)
  -- ^ Function parsing argument from a serialized message.
  -> (arg -> m msg)
  -- ^ Function un-parsing an argument into a serialized message.
  -> (Self m msg arg inst -> arg -> m ret)
  -- ^ Actual program to run from a specification.
  -> App m msg arg inst ret
app = App

-- | Typical function to run an 'App' with simplifying assumptions:
--
-- - locate the Self instance once for the rest of the program
-- - reads the arguments once
-- - starts the actual program
runApp
  :: Monad m
  => m inst
  -- ^ An action to locate the self instance of an applictaion.
  -- This action is run exactly once and the result is stored in 'Self'.
  -> m msg
  -- ^ An action to retrieve the message to start the application with.
  -- This action is run exactly once, the result is then parsed and discarded.
  -> App m msg arg inst ret
  -- ^ The 'App' to run
  -> m ret
runApp getInst getMsg (App parse unparse go) = do
    inst <- getInst
    let self = Self (pure inst) unparse
    arg <- parse =<< getMsg
    go self arg
