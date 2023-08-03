module Data.Allen.Interval ( interval
                           , fromID
                           , assume
                           , assumeSet
                           , assumeBits
                           , setRelation
                           , getConstraints
                           ) where

import Control.Monad.State

import Data.Allen.Types
import Data.Allen.Relation

import qualified Data.Map as Map

-- | Create a new interval. 
-- Returns the interval ID
interval :: Allen IntervalID 
interval = do
    intervals <- get

    let iD         = Map.size intervals
        iRelations = Map.fromList [(x, allRelationBits) | x <- [0 .. iD - 1]]
        intervals' = Map.map (\x -> setRelation x allRelationBits iD) intervals
        i          = Interval iD iRelations

    put $ Map.insert iD i intervals'
    return iD 

-- | Set the relations between two intervals 
setRelation :: Interval -> RelationBits -> IntervalID -> Interval 
setRelation i1 r i2 = i1 { intervalRelations = relations }
    where relations = Map.insert i2 r $ intervalRelations i1

-- | Define a relation between two intervals. 
assume :: IntervalID -> Relation -> IntervalID -> Allen ()
assume id1 r = assumeBits id1 (toBits r)

-- | Define a set of relations between two intervals
assumeSet :: IntervalID -> [Relation] -> IntervalID -> Allen ()
assumeSet id1 = assumeBits id1 . relationUnion . map toBits

-- | Define a relation between two intervals using RelationBits.
assumeBits :: IntervalID -> RelationBits -> IntervalID -> Allen ()
assumeBits id1 r id2 = do 
    i1 <- fromID id1 
    i2 <- fromID id2 

    let i1' = setRelation i1 r id2 
        i2' = setRelation i2 (converse r) id1

    updateIntervals [(id1, i1'), (id2, i2')]

-- | Return the set of possible constraints/relations between two intervals
getConstraints :: IntervalID -> IntervalID -> Allen RelationBits
getConstraints id1 id2 = Map.findWithDefault 0 id2 . intervalRelations <$> fromID id1

-- | Update intervals in the graph
updateIntervals :: [(IntervalID, Interval)] -> Allen ()
updateIntervals = mapM_ (\(iD, i) -> modify $ Map.insert iD i)
