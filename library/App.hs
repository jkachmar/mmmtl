module App where

-- Standard library
import           Protolude

-- External modules
import           Control.Exception.Safe      (MonadCatch, MonadThrow)
import           Control.Lens                (over, view)
import           Data.Generics.Product.Typed
import           Katip                       (Katip (..), KatipContext (..),
                                              LogContexts, LogEnv, Namespace)

-- Internal modules
import           Effects.Http
import           Effects.Logger
import           Effects.Scribe

--------------------------------------------------------------------------------
-- | The application type, a monad transformer that wires up all of the
-- | abstract interfaces for the application's behavior with concrete, real
-- | world implementations for a production environment.
newtype App a
  = App
  { unApp :: ReaderT Config IO a
  } deriving ( Functor, Applicative, Monad, MonadIO
             , MonadCatch, MonadThrow, MonadReader Config
             )

-- | Embed a function from `Config` to some arbitrary monad in `App`.
-- |
-- | This isn't _strictly_ necessary, but can be convenient; it really just
-- | translates `(Config ->)` into `ReaderT Config` within `App`.
mkApp :: (Config -> IO a) -> App a
mkApp = App . ReaderT

-- | Run the `App` transformer stack with the given configuration record.
runApp :: Config -> App a -> IO a
runApp config app = runReaderT (unApp app) config

--------------------------------------------------------------------------------
-- | Configuration context for the application. This is where all the
-- | operational configuration values are wired up on initialization.
data Config
  = Config
  { katipConfig :: KatipConfig
  , httpConfig  :: HttpConfig
  } deriving Generic

--------------------------------------------------------------------------------
-- Typeclass instances wiring up `Scribe` functionality and its dependencies.

-- | Provides an instance of `Katip` for `App`, so that the `Katip`-powered
-- | logging functions implemented in the `Scribe` instance can function
-- | properly.
instance Katip App where
  getLogEnv = view (typed @KatipConfig . typed @LogEnv)
  localLogEnv f (App m) =
    App $ local (over (typed @KatipConfig . typed @LogEnv) f) m

-- | Provides an instance of `KatipContext` for `App`, so that the `Katip`-
-- | powered logging functions implemented in the `Scribe` instance can function
-- | properly.
instance KatipContext App where
  getKatipContext = view (typed @KatipConfig . typed @LogContexts)
  localKatipContext f (App m) =
    App $ local (over (typed @KatipConfig . typed @LogContexts) f) m

  getKatipNamespace = view (typed @KatipConfig . typed @Namespace)
  localKatipNamespace f (App m) =
    App $ local (over (typed @KatipConfig . typed @Namespace) f) m

-- | Provides an instance of `Scribe` for `App` that uses `Katip` for its log
-- | actions.
instance Scribe App where
  info = katipScribeInfoImpl
  error = katipScribeErrorImpl
  trace = katipScribeTraceImpl
  pushNamespace = katipScribePushNamespaceImpl
  popNamespace = katipScribePopNamespaceImpl

--------------------------------------------------------------------------------
-- | Provides an instance of `Logger` for `App` that uses `Scribe` for its log
-- | actions.
instance Logger App where
  info = scribeLoggerInfoImpl
  error = scribeLoggerErrorImpl
  trace = scribeLoggerTraceImpl

--------------------------------------------------------------------------------
-- | Provides an instance of `Http` for `App` that uses the `httpConfig`
-- | provided by `App`'s operational configuration to execute HTTP client
-- | calls.
instance Http App where
  call = ioCallHttpImpl
