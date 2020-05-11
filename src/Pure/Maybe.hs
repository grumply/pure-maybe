module Pure.Maybe
  ( -- * Producers
    producing
  , producingKeyed
    -- * Consumers
  , consuming
  , consumingWith
    -- * consumingWith options
  , Options(..)
  , defaultOptions
  , delaying
  , suspense
  , trouble
  ) where

import Pure.Maybe.Consumer
import Pure.Maybe.Producer