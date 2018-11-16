module Run where

-- Standard library
import           Protolude

-- External modules
import           Katip                           (LogContexts, LogEnv,
                                                  Namespace)

-- Internal modules
import           App

import           Effects.Http                as Http
import           Effects.Logger              (Logger)
import qualified Effects.Logger              as Log
import           Effects.Scribe              (KatipConfig (..))

--------------------------------------------------------------------------------
-- | Example effectful function in our application, only defined in terms of
-- | the typeclass interfaces that describe the effects constraining it.
-- |
-- | Note that we _cannot_ perform arbitrary `IO` actions here as we have no
-- | access to `IO` or any typeclasses such as `MonadIO` or `MonadUnliftIO`.
effectfulFunction
  :: ( Http m
     , Logger m
     )
  => m ()
effectfulFunction = do
  -- This fails, as we've provided no `MonadIO` constraint
  -- liftIO $ putStrLn "Hello"

  -- Make some HTTP calls...
  httpResult :: Text <- call undefined undefined

  -- Log the HTTP response and timeshift with the logger
  Log.info (show httpResult)

--------------------------------------------------------------------------------
-- | Main entry point for our application where we performan configuration
run :: IO ()
run = do
  -- Set up the HTTP connection manager, this is an `IO` action
  httpManager :: Manager <- undefined
  let httpConfig = HttpConfig { httpManager }

  -- Set up Katip logging configuration, these may be `IO` actions
  contexts  :: LogContexts <- undefined
  logEnv    :: LogEnv      <- undefined
  namespace :: Namespace   <- undefined
  let katipConfig :: KatipConfig
        = KatipConfig
        { contexts
        , logEnv
        , namespace
        }

  -- Wire up application configuration in config record.
  let config = Config { httpConfig, katipConfig }

  -- Run the application
  runApp config effectfulFunction
