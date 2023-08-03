module Data.Allen.IntervalGraph (updateGraph) where

import Control.Monad.State 

import Data.Allen.Types

import qualified Data.Map as Map

updateGraph :: [(Int, Interval)] -> Allen ()
updateGraph = mapM_ (\(iD, i) -> modify $ Map.insert iD i)
