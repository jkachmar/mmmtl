module Effects.Http where

-- Standard library
import           Protolude                    hiding (HasField)

-- External modules
import           Control.Lens                 (view)
import           Data.Generics.Product.Typed

--------------------------------------------------------------------------------
class Monad m => Http m where
  call :: BaseUrl -> ClientM a -> m a

--------------------------------------------------------------------------------
-- | Concrete implementation of `Http`'s `call` method, using a connection
-- | `Manager` supplied by some operational context within `m`.
ioCallHttpImpl
  :: ( MonadReader context m
     , HasHttpConfig context
     , MonadIO m
     )
  => BaseUrl
  -> ClientM a
  -> m a
ioCallHttpImpl _burl _client = do
  _manager <- view (typed @HttpConfig . typed @Manager)
  undefined

-- | Configuration type for the `Http` class to supply a `Manager` that handles
-- | pooling and distributing HTTP handles.
data HttpConfig
  = HttpConfig
  { httpManager  :: Manager
  } deriving Generic

type HasHttpConfig context = HasType HttpConfig context

-- Stubbed out types
data BaseUrl
data ClientM a
data Manager
