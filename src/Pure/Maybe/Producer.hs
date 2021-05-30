{-# language AllowAmbiguousTypes, RankNTypes, 
      TypeApplications, RecordWildCards, 
      KindSignatures, ViewPatterns, 
      ScopedTypeVariables 
  #-}
module Pure.Maybe.Producer
  (-- * Producers
    producing
  , producingKeyed
  ) where

import Control.Concurrent

import Pure.Elm hiding (Left,Right,Start,Options,key)

import Data.Foldable
import Data.Traversable
import Data.Typeable

data Model a = Model
  { producer :: Maybe ThreadId
  , evitable :: Maybe a
  }

data Message a
  = Start
  | Receive
  | Eventuated a
  | Shutdown

-- | Manage the production of a `Maybe a` value.
--
-- > producing someNetworkRequest (consuming someResponseViewer)
--
producing 
  :: forall a. Typeable a
  => IO a 
  -> (Maybe a -> View) 
  -> View
producing io = run (App [Start] [Receive] [Shutdown] (pure mdl) update view)
  where
    mdl = Model Nothing Nothing
 
    update :: Elm (Message a) 
           => Message a 
           -> (Maybe a -> View) 
           -> Model a 
           -> IO (Model a)
    update Start _ Model {..} = do
      producer <- Just <$> forkIO (io >>= command . Eventuated)
      pure Model {..}

    update (Eventuated e) _ mdl =
      pure mdl { evitable = Just e }

    update Receive _ Model {..} = 
      pure Model {..}

    update Shutdown _ Model {..} =
      case producer of
        Just tid -> do
          killThread tid
          pure Model {..}
        _ -> 
          pure Model {..}

    view f Model {..} = f evitable

data KeyedModel key a = KeyedModel
  { key :: key
  , keyedProducer :: Maybe ThreadId 
  , keyedEvitable :: Maybe a
  }

-- | Manage the production of a keyed `Maybe a` value.
--
-- > producingKeyed someRequestData someNetworkRequest (consuming someResponseViewer)
--
producingKeyed
  :: forall key a. (Eq key, Typeable key, Typeable a) 
  => key
  -> (key -> IO a)
  -> (key -> Maybe a -> View) 
  -> View
producingKeyed k p f = run (App [Start] [Receive] [Shutdown] (pure mdl0) update view) (k,p,f)
  where
    mdl0 = KeyedModel k Nothing Nothing

    update :: Elm (Message a) 
           => Message a 
           -> (key,key -> IO a,key -> Maybe a -> View) 
           -> KeyedModel key a 
           -> IO (KeyedModel key a)
    update Start (_,p,_) mdl = do
      mtid <- Just <$> forkIO (p (key mdl) >>= command . Eventuated)
      pure mdl { keyedProducer = mtid }

    update (Eventuated (Just -> e)) _ mdl =
      pure (KeyedModel (key mdl) Nothing e)

    update Receive (k,p,_) mdl
      | k == key mdl = pure mdl
      | otherwise = do
        for_ (keyedProducer mdl) killThread
        mtid <- Just <$> forkIO (p k >>= command . Eventuated)
        pure (KeyedModel k mtid Nothing) 

    update Shutdown _ KeyedModel {..} =
      case keyedProducer of
        Just tid -> do
          killThread tid
          let keyedProducer = Nothing
          pure KeyedModel {..}
        _ -> 
          pure KeyedModel {..}

    view (_,_,f) KeyedModel {..} =
      f key keyedEvitable
