module Test.LoggingTest where

-- Standard library
import           Protolude

-- External modules
import           Data.IORef
import           Test.Tasty.Hspec

-- Internal library modules
import           Effects.Logger (Logger)
import qualified Effects.Logger as Log

-- Internal test modules
import           Test.Mocks.Logger
import           Test.Mocks.Scribe
import           Test.TestApp

--------------------------------------------------------------------------------
spec_app1_logger :: Spec
spec_app1_logger = do
  describe "logging" $ do
    context "when enabled with an enabled scribe" $ do
      it "should accumulate logs in the correct channels" $ do
        -- Construct an empty `LoggerState` to hold the accumulated loglines
        scribeState <- newIORef $ ListScribeState mempty mempty mempty mempty
        let config = LogState scribeState

        -- Use TypeApplications to provide the correct wiring to `runTestApp`
        runTestApp @TestScribeLogger @TestScribeList config logTest

        outputState <- readIORef scribeState
        outputState `shouldBe` enabledLogTestFixture

    context "when enabled with a disabled scribe" $ do
      it "should accumulate no logs in any channel" $ do
        -- Construct an empty `LoggerState` to hold the accumulated loglines
        scribeState <- newIORef $ ListScribeState mempty mempty mempty mempty
        let config = LogState scribeState

        -- Use TypeApplications to provide the correct wiring to `runTestApp`
        runTestApp @TestScribeLogger @TestNoScribe config logTest

        outputState <- readIORef scribeState
        outputState `shouldBe` disabledLogTestFixture

    context "when disabled with an enabled scribe" $ do
      it "should accumulate no logs in any channel" $ do
        -- Construct an empty `LoggerState` to hold the accumulated loglines
        scribeState <- newIORef $ ListScribeState mempty mempty mempty mempty
        let config = LogState scribeState

        -- Use TypeApplications to provide the correct wiring to `runTestApp`
        runTestApp @TestNoLogger @TestScribeList config logTest

        outputState <- readIORef scribeState
        outputState `shouldBe` disabledLogTestFixture

--------------------------------------------------------------------------------
-- | An example function using the `Logger` effect.
logTest :: Logger m => m ()
logTest = do
  Log.info  "Hello from INFO!"
  Log.error "Hello from ERROR!"
  Log.trace "Hello from TRACE!"

-- | An example logging state in which loglines will be accumulated.
data LogState
  = LogState
  { listScribeState :: IORef ListScribeState
  } deriving Generic

-- | Fixture for a successful run of `logTest` when logging is enabled.
enabledLogTestFixture :: ListScribeState
enabledLogTestFixture =
  let infoList      = ["Hello from INFO!"]
      errorList     = ["Hello from ERROR!"]
      traceList     = ["Hello from TRACE!"]
      namespaceList = []
  in ListScribeState { infoList, errorList, traceList, namespaceList }

-- | Fixture for a successful run of `logTest` when logging is disabled.
disabledLogTestFixture :: ListScribeState
disabledLogTestFixture = ListScribeState [] [] [] []
