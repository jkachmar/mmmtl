module Test.Mocks.Scribe where

-- Standard library
import           Protolude

-- External modules
import           Control.Lens
import           Data.Generics.Product.Fields
import           Data.Generics.Product.Typed
import           Data.IORef

--------------------------------------------------------------------------------
-- | Disambiguate between desired instances when wiring up `Scribe`.
data TestScribe
  = TestNoScribe     -- ^ Indicate that the `Scribe` should drop all logs.
  | TestScribeList   -- ^ Indicate that the `Scribe` should log to a list.

-- | Newtype wrapper around `[Text]`, representing an append-only list of
-- | loglines created by the mocked `Scribe`.
data ListScribeState
  = ListScribeState
  { infoList      :: [Text]
  , errorList     :: [Text]
  , traceList     :: [Text]
  , namespaceList :: [Text]
  } deriving (Generic, Show, Eq)

-- | Type alias for a constraint enforcing that some operational context must
-- | provide a type with a member of type `IORef ListScribeState`.
-- |
-- | This constraint gives us the means to enforce the fact that our list-based,
-- | mocked logger has access to an `IORef ListScribeState` by which it can
-- | accumulate its state over the course of the test run.
type HasListScribe context = HasType (IORef ListScribeState) context

--------------------------------------------------------------------------------
-- Mocked implementations for `Scribe`'s interface that append loglines to some
-- shared state.

listScribeInfoImpl
  :: (MonadIO m, MonadReader r m, HasListScribe r)
  => Text -> m ()
listScribeInfoImpl msg = do
    ioScribeState <- view (typed @(IORef ListScribeState))
    liftIO
      $  modifyIORef ioScribeState
      $  field' @"infoList"
      %~ \(l :: [Text]) -> l <> [msg]

listScribeErrorImpl
  :: (MonadIO m, MonadReader r m, HasListScribe r)
  => Text -> m ()
listScribeErrorImpl msg = do
    ioScribeState <- view (typed @(IORef ListScribeState))
    liftIO
      $  modifyIORef ioScribeState
      $  field' @"errorList"
      %~ \(l :: [Text]) -> l <> [msg]

listScribeTraceImpl
  :: (MonadIO m, MonadReader r m, HasListScribe r)
  => Text -> m ()
listScribeTraceImpl msg = do
    ioScribeState <- view (typed @(IORef ListScribeState))
    liftIO
      $  modifyIORef ioScribeState
      $  field' @"traceList"
      %~ \(l :: [Text]) -> l <> [msg]

listScribePushNamespaceImpl
  :: (MonadIO m, MonadReader r m, HasListScribe r)
  => Text -> m ()
listScribePushNamespaceImpl msg = do
    ioScribeState <- view (typed @(IORef ListScribeState))
    liftIO
      $  modifyIORef ioScribeState
      $  field' @"namespaceList"
      %~ \(l :: [Text]) -> [msg] <> l

listScribePopNamespaceImpl
  :: (MonadIO m, MonadReader r m, HasListScribe r)
  => m ()
listScribePopNamespaceImpl = do
    ioScribeState <- view (typed @(IORef ListScribeState))
    liftIO
      $  modifyIORef ioScribeState
      $  field' @"namespaceList"
      %~ \(l :: [Text]) -> drop 1 l

--------------------------------------------------------------------------------
-- Mocked implementations for `Scribe`'s interface that discard all loglines.

noScribeInfoImpl :: Monad m => Text -> m ()
noScribeInfoImpl _ = pure ()

noScribeErrorImpl :: Monad m => Text -> m ()
noScribeErrorImpl _ = pure ()

noScribeTraceImpl :: Monad m => Text -> m ()
noScribeTraceImpl _ = pure ()

noScribePushNamespaceImpl :: Monad m => Text -> m ()
noScribePushNamespaceImpl _ = pure ()

noScribePopNamespaceImpl :: Monad m => m ()
noScribePopNamespaceImpl = pure ()
