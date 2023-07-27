module Data.Allen ( module Data.Allen.Types 
                  , module Data.Allen.Interval
                  , module Data.Allen.Relation
                  , execAllen
                  , evalAllen
                  , runAllen
                  ) where

import Control.Monad.State

import Data.Allen.Types
import Data.Allen.Interval
import Data.Allen.Relation

-- | Same as runAllenState, but discards the final value
execAllen :: Allen a -> IntervalGraph
execAllen = snd . runAllen

-- | Same as runAllen, but discards the final state
evalAllen :: Allen a -> a
evalAllen = fst . runAllen

-- | Runs an allen computation starting with an inital empty graph.
-- Returns the resulting graph and the final state
runAllen :: Allen a -> (a, IntervalGraph)
runAllen = flip runState []
