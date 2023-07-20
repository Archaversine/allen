module Data.Allen.Interval (interval, constrain, constraints) where

import Control.Monad.State

import Data.Allen.Types
import Data.Allen.Relation

import qualified Data.Vector as V

-- | Create a new interval. 
interval :: Allen Interval 
interval = do
    intervals <- get

    let i = Interval (V.length intervals) []

    put $ V.snoc intervals i 
    return i 

-- | Add a relation to an interval
-- Ensures no duplicates are created
addRelation :: Interval -> Relation -> Interval -> Interval 
addRelation i1 r i2 = i1 { intervalRelations = (r, i2) : filtered }
    where filtered = filter (/= (r, i2)) (intervalRelations i2)

-- | Define a relation between two intervals. 
constrain :: Interval -> Relation -> Interval -> Allen ()
constrain i1 r i2 = do 
    let i1' = addRelation i1 r i2 
        i2' = addRelation i2 (inverse r) i1

    modify (V.// [(intervalID i1, i1'), (intervalID i2, i2')])

constraints :: Interval -> Interval -> [IntervalConstraint]
constraints i1 i2 = intervalRelations i1

