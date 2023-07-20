module Data.Allen ( module Data.Allen.Types 
                  , module Data.Allen.Interval
                  , module Data.Allen.Relation
                  , runAllen
                  , runAllenState
                  ) where

import Control.Monad.State

import Data.Allen.Types
import Data.Allen.Interval
import Data.Allen.Relation

import qualified Data.Vector as V

-- | Same as runAllenState, but discards the final state
runAllen :: Allen a -> IntervalGraph
runAllen = snd . runAllenState

-- | Runs an allen computation starting with an inital empty graph.
-- Returns the resulting graph and the final state
runAllenState :: Allen a -> (a, IntervalGraph)
runAllenState = flip runState V.empty
