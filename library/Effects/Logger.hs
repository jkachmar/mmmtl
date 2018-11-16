module Effects.Logger where

-- Standard library
import           Protolude

-- Internal modules
import           Effects.Scribe (Scribe)
import qualified Effects.Scribe as Scribe

--------------------------------------------------------------------------------
class Monad m => Logger m where
  info  :: Text -> m ()
  error :: Text -> m ()
  trace :: Text -> m ()

--------------------------------------------------------------------------------
-- Concrete implementations of the `Logger` method for a given `Scribe` backend.
--
-- These could go in a `Logger.Scribe` module to keep the interface declaration
-- module clean.
--
-- Note that since the _entire_ implementation here is deferred to `Scribe`, we
-- can swap out backends or modify behavior simply by changing the `Scribe`
-- typeclass instance implementations in the top-level wiring type.

-- | Concrete implementation of `Logger`'s `info` method for a given `Scribe`.
scribeLoggerInfoImpl :: Scribe m => Text -> m ()
scribeLoggerInfoImpl = Scribe.info

-- | Concrete implementation of `Logger`'s `error` method for a given `Scribe`.
scribeLoggerErrorImpl :: Scribe m => Text -> m ()
scribeLoggerErrorImpl = Scribe.error

-- | Concrete implementation of `Logger`'s `trace` method for a given `Scribe`.
scribeLoggerTraceImpl :: Scribe m => Text -> m ()
scribeLoggerTraceImpl = Scribe.trace
