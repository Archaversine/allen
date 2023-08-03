module Data.Allen.Interval ( interval
                           , fromID
                           , assume
                           , assumeBits
                           , getConstraints
                           ) where

import Control.Monad.State

import Data.Allen.Types
import Data.Allen.Relation
import Data.Allen.IntervalGraph

import Data.Bits

import qualified Data.Map as Map

-- | Create a new interval. 
-- Returns the interval ID
interval :: Allen IntervalID 
interval = do
    intervals <- get

    let iD         = length intervals
        iRelations = Map.fromList [(x, allRelationBits) | x <- [0 .. iD - 1]]
        intervals' = [addRelation x allRelationBits iD  | x <- intervals]
        i          = Interval iD iRelations

    put $ intervals' <> [i]
    return iD 

-- | Add a relation to an interval
-- Ensures no duplicates are created
addRelation :: Interval -> RelationBits -> IntervalID -> Interval 
addRelation i1 r i2  = i1 { intervalRelations = relations' }
    where relations  = intervalRelations i1 
          relation   = Map.findWithDefault 0 i2 relations
          relations' = Map.insert i2 (relation .|. r) relations

-- | Define a relation between two intervals. 
assume :: IntervalID -> Relation -> IntervalID -> Allen ()
assume id1 = assumeBits id1 . toBits

-- | Define a relation between two intervals using RelationBits.
assumeBits :: IntervalID -> RelationBits -> IntervalID -> Allen ()
assumeBits id1 r id2 = do 
    i1 <- fromID id1 
    i2 <- fromID id2 

    let i1' = addRelation i1 r id2 
        i2' = addRelation i2 (converse r) id1

    updateGraph [(id1, i1'), (id2, i2')]

getConstraints :: IntervalID -> IntervalID -> Allen RelationBits
getConstraints id1 id2 = Map.findWithDefault 0 id2 . intervalRelations <$> fromID id1
