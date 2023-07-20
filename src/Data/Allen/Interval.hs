module Data.Allen.Interval (interval, constrain) where

import Control.Monad.State

import Data.Allen.Types
import Data.Allen.Relation

import qualified Data.Map.Strict as M

-- | Create a new interval. 
interval :: Allen Interval 
interval = do
    intervals <- get

    let n = M.size intervals
        i = Interval n []

    put $ M.insert n i intervals

    return i

-- | Add a relation to an interval
-- Ensures no duplicates are created
addRelation :: Interval -> Relation -> Interval -> Interval 
addRelation i1 r i2 = i1 { intervalRelations = (r, i2) : filtered }
    where filtered = filter (/= (r, i2)) (intervalRelations i2)

-- | Define a relation between two intervals. 
constrain :: Interval -> Relation -> Interval -> Allen ()
constrain i1 r i2 = do 
    modify $ M.adjust (\i -> addRelation i r i2) (intervalID i1) 
    modify $ M.adjust (\i -> addRelation i (inverse r) i1) (intervalID i2)

