
-- | Helper module to deal with App where args are {de-,}serialized using GHC 
-- static pointers.
module System.Process.Corecursive.Closure where

import           Control.Distributed.Closure (Closure, unclosure)
import qualified Data.Binary                 as Binary
import qualified Data.ByteString.Base64.Lazy as B64
import           Data.ByteString.Lazy        (ByteString)
import           Data.Typeable               (Typeable)

-- | Helper to deal with App when you want to use Closures as a
parseClosureB64
  :: Typeable a
  => ByteString
  -> Closure a
parseClosureB64 b64 = do
    let bstr = B64.decode b64
    let encodedClosure = either (error "invalid base64") id bstr
    Binary.decode encodedClosure

-- | Dual to 'opClosureFromB64'.
unparseClosureB64
  :: Typeable a
  => Closure a
  -> ByteString
unparseClosureB64 clo =
    B64.encode $ Binary.encode clo
