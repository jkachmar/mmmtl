module Effects.Scribe where

-- Standard library
import           Protolude

-- External modules
import           Data.Generics.Product.Typed
import           Katip

--------------------------------------------------------------------------------
-- | Typeclass representing an interface for logging `Scribe`s.
class Monad m => Scribe m where
  info          :: Text -> m ()
  error         :: Text -> m ()
  trace         :: Text -> m ()
  pushNamespace :: Text -> m ()
  popNamespace  :: m ()

--------------------------------------------------------------------------------
-- Concrete implementations of the `Scribe` methods for `Katip`.
--
-- These could go in a `Scribe.Katip` module to keep the interface declaration
-- module clean.
--
-- These functions are naively implemented purely in terms of `KatipContext`,
-- which the concrete application type will be expected to implement an instance
-- of.
--
-- However, because these are _just_ concrete implementations of the methods
-- described in the `Scribe` typeclass, we're free to swap out the constraints
-- however we see fit (e.g. if we want to implement our own `StateT`-based system
-- for tracking `Namespace`s and `LogContext`s).

-- | Concrete implementation of `Scribe`'s `info` method for a `Katip`-powered
-- | logger.
-- |
-- | Logs in `Katip`'s `Info` context, automatically gathering module and source
-- | code location via the `logTM` Template Haskell splice.
katipScribeInfoImpl :: (MonadIO m, KatipContext m) => Text -> m ()
katipScribeInfoImpl = logFM InfoS . ls

-- | Concrete implementation of `Scribe`'s `error` method for a `Katip`-powered
-- | logger.
-- |
-- | Logs in `Katip`'s `Error` context.
katipScribeErrorImpl :: (MonadIO m, KatipContext m) => Text -> m ()
katipScribeErrorImpl = logFM ErrorS . ls

-- | Concrete implementation of `Scribe`'s `trace` method for a `Katip`-powered
-- | logger.
-- |
-- | Logs in `Katip`'s `Debug` context.
katipScribeTraceImpl :: (MonadIO m, KatipContext m) => Text -> m ()
katipScribeTraceImpl = logFM DebugS . ls

-- | Concrete implementation of `Scribe`'s `pushNamespace` method for a `Katip`-
-- | powered logger.
katipScribePushNamespaceImpl :: (MonadIO m, KatipContext m) => Text -> m ()
katipScribePushNamespaceImpl = undefined

-- | Concrete implementation of `Scribe`'s `popNamespace` method for a `Katip`-
-- | powered logger.
katipScribePopNamespaceImpl :: (MonadIO m, KatipContext m) => m ()
katipScribePopNamespaceImpl = undefined

-- | Configuration record for a `Katip`-powered logger.
-- |
-- | While not used directly in this module, it will be used to provide the
-- | application's logging configuration when the typeclass instances are wired
-- | up.
data KatipConfig
  = KatipConfig
  { contexts  :: !LogContexts
  , logEnv    :: !LogEnv
  , namespace :: !Namespace
  } deriving Generic

-- | Type alias for a constraint enforcing that some operational context must
-- | provide a type with a member of type `KatipConfig`
-- |
-- | This constraint gives us the means to enforce the fact that our `Katip`-
-- | powered logger _must_ execute in an environment providing the appropriate
-- | configuration.
type HasKatipConfig context = HasType KatipConfig context
