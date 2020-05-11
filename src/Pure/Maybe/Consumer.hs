{-# language AllowAmbiguousTypes, RankNTypes, 
      TypeApplications, RecordWildCards, 
      KindSignatures, ViewPatterns, 
      ScopedTypeVariables, LambdaCase
  #-}
module Pure.Maybe.Consumer 
  ( -- * Consumers
    consuming
  , consumingWith
    -- * consumingWith options
  , Options
  , defaultOptions
  , delaying
  , suspense
  , trouble
  ) where

import Control.Concurrent

import Pure.Elm hiding (Left,Right,Start,Options,key,modify,get,start)

import Data.Foldable
import Data.Traversable
import Data.Typeable

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

-- | The simplest `Maybe` viewer. Equivalent to `maybe Null`.
consuming :: (a -> View) -> Maybe a -> View
consuming = maybe Null

data Env a = Env
  { options :: Options
  , viewer :: a -> View
  , eventual :: Maybe a
  }

data Model = Model 
  { start           :: Time
  , delayer         :: Maybe ThreadId
  , suspenseMonitor :: Maybe ThreadId 
  , troubleMonitor  :: Maybe ThreadId
  , current         :: View
  }

data Message a 
  = Start 
  | Receive 
  | Ready a ThreadId
  | Suspense ThreadId 
  | Trouble ThreadId

type ConsumerM a = ReaderT (Env a) (StateT Model IO)

-- | A `Maybe` viewer with options for intermediate display breakpoints. The 
-- options support delaying, suspense and trouble breakpoints.
--
-- > loader :: (a -> View) -> Maybe a -> View
-- > loader = 
-- >   consumingWith 
-- >     ( defaultOptions 
-- >     & delaying (Milliseconds 300 0) beforeLoadingView
-- >     & suspense (Milliseconds 500 0) someLoadingSpinner
-- >     & trouble (Seconds 5 0) someDefaultTroubleView
-- >     ) 
-- >
-- > f :: forall a. Maybe a -> View
-- > f = loader (someConsumer :: a -> View)
consumingWith 
  :: forall a. (Typeable a) 
  => Options 
  -> (a -> View) 
  -> Maybe a 
  -> View
consumingWith = \os f ma -> 
    run (App [Start] [Receive] [] mdl exec view) (Env os f ma)
  where
    mdl = Model 0 Nothing Nothing Nothing Null
    exec :: Elm (Message a) => Message a -> Env a -> Model -> IO Model
    exec msg env mdl = execStateT (runReaderT (update msg) env) mdl
    view _ = current

setStart :: ConsumerM a ()
setStart = do
  now <- liftIO time
  lift $ modify $ \cm -> cm 
    { start = now }

setCurrent :: View -> ConsumerM a ()
setCurrent v = lift $ modify $ \m -> m { current = v }

-- Fork a thread that delays for a given duration from a starting time and
-- then dispatches a given message.
monitor :: Elm (Message a) => Time -> (ThreadId -> Message a) -> ConsumerM a ThreadId
monitor d msg = do
  s <- lift $ gets start
  elapsed <- subtract s <$> liftIO time
  let 
    delay' t
      | t > 0 = delay t
      | otherwise = pure ()
  liftIO $ forkIO $ do
    tid <- myThreadId
    delay' (d - elapsed)
    command (msg tid)

startSuspenseMonitor :: Elm (Message a) => ConsumerM a ()
startSuspenseMonitor =
  asks (_suspense . options) >>= \case
    Nothing    -> pure ()
    Just (t,_) -> do
      tid <- monitor t Suspense 
      lift $ modify $ \m -> m 
        { suspenseMonitor = Just tid }

stopSuspenseMonitor :: ConsumerM a ()
stopSuspenseMonitor = do
  m <- lift get 
  traverse_ (liftIO . killThread) (suspenseMonitor m) 
  lift $ put m { suspenseMonitor = Nothing }

setSuspenseView :: ConsumerM a ()
setSuspenseView =
  asks (_suspense . options) >>= \case
    Nothing -> pure ()
    Just (_,v) -> setCurrent v 

startTroubleMonitor :: Elm (Message a) => ConsumerM a ()
startTroubleMonitor =
  asks (_trouble . options) >>= \case
    Nothing    -> pure ()
    Just (t,_) -> do
      tid <- monitor t Trouble 
      lift $ modify $ \m -> m 
        { suspenseMonitor = Just tid }

stopTroubleMonitor :: Elm (Message a) => ConsumerM a ()
stopTroubleMonitor = do
  m <- lift get 
  traverse_ (liftIO . killThread) (troubleMonitor m) 
  lift $ put m { troubleMonitor = Nothing }

setTroubleView :: ConsumerM a ()
setTroubleView =
  asks (_trouble . options) >>= \case
    Nothing -> pure ()
    Just (_,v) -> setCurrent v

setDelayView :: ConsumerM a ()
setDelayView =
  asks (_delaying . options) >>= \case
    Nothing -> pure ()
    Just (_,v) -> setCurrent v

startDelayer :: Elm (Message a) => a -> ConsumerM a ()
startDelayer a =
  asks (_delaying . options) >>= \case
    Nothing -> pure ()
    Just (t,_) -> do
      tid <- monitor t (Ready a)
      lift $ modify $ \m -> m
        { delayer = Just tid }

stopDelayer :: ConsumerM a ()
stopDelayer =
  lift (gets delayer) >>= \case
    Nothing -> pure ()
    Just tid -> do
      liftIO (killThread tid)
      lift $ modify $ \m -> m
        { delayer = Nothing }

update :: Elm (Message a) => Message a -> ConsumerM a ()
update = \case
  Start -> do
    setStart
    startSuspenseMonitor
    startTroubleMonitor
    setDelayView

  Receive -> do
    stopSuspenseMonitor
    stopTroubleMonitor
    stopDelayer
    asks eventual >>= \case
      Just a -> startDelayer a
      _      -> update Start

  Suspense tid ->
    lift (gets suspenseMonitor) >>= \case
      Just ((== tid) -> True) ->
        asks eventual >>= \case
          Just _  -> pure ()
          Nothing -> setSuspenseView
      _ -> pure ()

  Trouble tid ->
    lift (gets troubleMonitor) >>= \case
      Just ((== tid) -> True) -> 
        asks eventual >>= \case
          Just _  -> pure ()
          Nothing -> setTroubleView
      _ -> pure ()

  Ready a tid ->
    lift (gets delayer) >>= \case
      Just ((== tid) -> False) -> pure ()
      _ -> do
        stopSuspenseMonitor
        stopTroubleMonitor
        f <- asks viewer
        lift $ modify $ \m -> m { current = f a }

-- | Configuration options for `consumingWith`.
data Options = Options
  { -- | Delay a view, even if a resource has arrived. Useful for
    -- avoiding flashes of content or the guarantee that a _suspense
    -- view is shown. This is a hostile approach, and should be used 
    -- very carefully.
    _delaying :: Maybe (Time,View)
    
  , -- | Suspense View to display after given Time. 
    -- Often a loading spinner, or similar.
    _suspense :: Maybe (Time,View) 

  , -- | Trouble View to display after a given Time.
    -- Often a message saying there is trouble talking to a server.
    _trouble  :: Maybe (Time,View)
  }

-- | Default options with no suspense view and no trouble view.
defaultOptions :: Options
defaultOptions = Options Nothing Nothing Nothing

-- | Add a delay to the rendering of an eventuated value.
-- A non-zero Time is considered user-hostile, and should be
-- considered carefully.
delaying :: Time -> View -> Options -> Options
delaying t v os = os { _delaying = Just (t,v) }

-- | Add a suspense timing and view to an Options.
suspense :: Time -> View -> Options -> Options
suspense t v os = os { _suspense = Just (t,v) }

-- | Add a trouble timing and view to an Options.
trouble :: Time -> View -> Options -> Options
trouble t v os = os { _trouble = Just (t,v) }