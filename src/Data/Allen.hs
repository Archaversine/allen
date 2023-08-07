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

import qualified Data.Map.Strict as Map

-- | Return the resulting interval graph of an allen computation
execAllen :: Allen a -> IntervalGraph
execAllen = snd . runAllen

-- | Return the result of an allen computation
evalAllen :: Allen a -> a
evalAllen = fst . runAllen

-- | Return the result of an allen computation and the resulting interval graph
runAllen :: Allen a -> (a, IntervalGraph)
runAllen = flip runState Map.empty
