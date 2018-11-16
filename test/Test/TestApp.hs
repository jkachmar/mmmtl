{-# LANGUAGE UndecidableInstances #-}

module Test.TestApp where

-- Standard library
import           Protolude

-- Internal library modules
import           Effects.Logger           (Logger)
import qualified Effects.Logger           as Logger
import           Effects.Scribe           (Scribe)
import qualified Effects.Scribe           as Scribe

-- Internal test modules
import           Test.Mocks.Logger
import           Test.Mocks.Scribe

--------------------------------------------------------------------------------
-- | Our monolithic testing type, containing sum types to help disambiguate
-- | between instances that should be used to mock different subsets of the
-- | overall application.
newtype TestApp
  (logger :: TestLogger)
  (scribe :: TestScribe)
  config
  a
  = TestApp
  { unTestApp :: ReaderT config IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader config)

-- | Run the `App` transformer stack with the given configuration record.
runTestApp
  :: forall logger scribe config a
   . config
  -> TestApp logger scribe config a
  -> IO a
runTestApp config app = runReaderT (unTestApp app) config

--------------------------------------------------------------------------------
-- | Provides an instance of `Scribe` for `TestApp` that accumulates loglines
-- | as lists of `Text` in the `ListScribeState` record.
instance HasListScribe config =>
  Scribe (TestApp logger TestScribeList config) where
  info = listScribeInfoImpl
  error = listScribeErrorImpl
  trace = listScribeTraceImpl
  pushNamespace = listScribePushNamespaceImpl
  popNamespace = listScribePopNamespaceImpl

-- | Provides an instance of `Scribe` for `TestApp` that drops all logs.
instance Scribe (TestApp logger TestNoScribe config) where
  info = noScribeInfoImpl
  error = noScribeErrorImpl
  trace = noScribeTraceImpl
  pushNamespace = noScribePushNamespaceImpl
  popNamespace = noScribePopNamespaceImpl

--------------------------------------------------------------------------------
-- | Provides an instance of `Logger` for `TestApp` that defers its logging to
-- | the provided `Scribe`.
-- |
-- | Note that since the _production_ `Logger` wires itself up to `Scribe`, we
-- | can reuse those wiring functions here.
instance ( Scribe (TestApp TestScribeLogger scribe config)
         , HasListScribe config
         ) => Logger (TestApp TestScribeLogger scribe config) where
  info = Logger.scribeLoggerInfoImpl
  error = Logger.scribeLoggerErrorImpl
  trace = Logger.scribeLoggerTraceImpl

-- | Provides an instance of `Logger` for `TestApp` that drops all logs.
instance Logger (TestApp TestNoLogger scribe config) where
  info = noLoggerInfoImpl
  error = noLoggerErrorImpl
  trace = noLoggerTraceImpl
