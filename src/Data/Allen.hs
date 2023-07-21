module Data.Allen ( module Data.Allen.Types 
                  , module Data.Allen.Interval
                  , module Data.Allen.Relation
                  , runAllen
                  , evalAllen
                  , runAllenState
                  ) where

import Control.Monad.State

import Data.Allen.Types
import Data.Allen.Interval
import Data.Allen.Relation

import qualified Data.Vector as V

-- | Same as runAllenState, but discards the final value
runAllen :: Allen a -> IntervalGraph
runAllen = snd . runAllenState

-- | Same as runAllen, but discards the final state
evalAllen :: Allen a -> a
evalAllen = fst . runAllenState

-- | Runs an allen computation starting with an inital empty graph.
-- Returns the resulting graph and the final state
runAllenState :: Allen a -> (a, IntervalGraph)
runAllenState = flip runState V.empty
