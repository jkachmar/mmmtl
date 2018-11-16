module Test.Mocks.Logger where

-- Standard library
import           Protolude

--------------------------------------------------------------------------------
-- | Disambiguate between desired instances when wiring up `Logger`.
data TestLogger
  = TestNoLogger     -- ^ Indicate that the `Logger` should drop all logs.
  | TestScribeLogger -- ^ Indicate that the `Logger` should log via a `Scribe`.

--------------------------------------------------------------------------------
-- Mocked implementations for `Scribe`'s interface that append loglines to some
-- shared state.

noLoggerInfoImpl :: Monad m => Text -> m ()
noLoggerInfoImpl _ = pure ()

noLoggerErrorImpl :: Monad m => Text -> m ()
noLoggerErrorImpl _ = pure ()

noLoggerTraceImpl :: Monad m => Text -> m ()
noLoggerTraceImpl _ = pure ()
