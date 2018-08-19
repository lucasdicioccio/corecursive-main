
module System.Process.Corecursive.Types
    ( App (..)
    , app
    , Self (..)
    ) where

-- | A datatype representing an instance of the (co-)recursive program.
data Self t inst msg arg = Self {
    executable :: t inst
  -- ^ An  action to retrieve an instance of the (co-)recursive program
  -- the 'inst' type is leaved as an argument to allow representing cases where
  -- the execution is actually remote.
  , unparse    :: arg -> t msg
  -- ^ An action to unparse arguments. Typically from 'unparseArgs'.
  }

-- | A datatype wrapping everything needed to make a (co-)recursive main
-- function.
data App t inst msg arg = App {
    parseArgs          :: msg -> t arg
  , unparseArgs        :: arg -> t msg
  , corecursiveProgram :: Self t inst msg arg -> arg -> t ()
  }

-- | Constructor for an App.
app
  :: (msg -> m arg)
  -- ^ Function parsing argument from a serialized message.
  -> (arg -> m msg)
  -- ^ Function un-parsing an argument into a serialized message.
  -> (Self m inst msg arg -> arg -> m ())
  -- ^ Actual program to run from a specification.
  -> App m inst msg arg
app = App
