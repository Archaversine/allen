module Data.Allen.Interval ( interval
                           , fromID
                           , constrain
                           , constraints
                           , inconsistent
                           ) where

import Control.Monad (when)
import Control.Monad.State

import Data.Allen.Types
import Data.Allen.Relation

import Data.Vector ((!))

import qualified Data.Vector as V

-- | Create a new interval. 
-- Returns the interval ID
interval :: Allen IntervalID 
interval = do
    intervals <- get

    let n = V.length intervals
        i = Interval n []

    put $ V.snoc intervals i 
    return n 

fromID :: IntervalID -> Allen Interval 
fromID n = gets (! n)

-- | Add a relation to an interval
-- Ensures no duplicates are created
addRelation :: Interval -> Relation -> IntervalID -> Interval 
addRelation i1 r i2 = i1 { intervalRelations = (r, i2) : filtered }
    where filtered = filter (/= (r, i2)) (intervalRelations i1)

-- | Define a relation between two intervals. 
constrain :: IntervalID -> Relation -> IntervalID -> Allen ()
constrain id1 r id2 = do 
    i1 <- fromID id1 
    i2 <- fromID id2

    let i1' = addRelation i1 r id2 
        i2' = addRelation i2 (inverse r) id1

    modify (V.// [(id1, i1'), (id2, i2')])

constraints :: IntervalID -> IntervalID -> Allen [IntervalConstraint]
constraints id1 id2 = do 
    i1 <- fromID id1 

    return $ filter ((== id2) . snd) $ intervalRelations i1
    
inconsistent :: Interval -> Bool
inconsistent i1 = any (`elem` [Precedes, Meets, PrecededBy, MetBy]) relations
    where relations = map fst $ intervalRelations i1
