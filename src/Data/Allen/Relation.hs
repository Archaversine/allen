module Data.Allen.Relation ( inverse
                           , hasConstraint
                           , precedes
                           , meets
                           , overlaps 
                           , finishedBy 
                           , contains
                           , starts 
                           , equals 
                           , startedBy 
                           , during 
                           , finishes 
                           , overlappedBy 
                           , metBy 
                           , precededBy
                           ) where

import Data.Allen.Types

inverse :: Relation -> Relation
inverse r = snd $ head $ filter ((== r) . fst) zipped
    where zipped = zip [minBound..] (reverse [minBound..])

hasConstraint :: Relation -> IntervalID -> IntervalID -> Allen Bool
hasConstraint r a b = do 
    i1 <- fromID a
    return $ (r, b) `elem` intervalRelations i1

precedes :: IntervalID -> IntervalID -> Allen Bool 
precedes = hasConstraint Precedes

meets :: IntervalID -> IntervalID -> Allen Bool 
meets = hasConstraint Meets 

overlaps :: IntervalID -> IntervalID -> Allen Bool 
overlaps = hasConstraint Overlaps 

finishedBy :: IntervalID -> IntervalID -> Allen Bool 
finishedBy = hasConstraint FinishedBy 

contains :: IntervalID -> IntervalID -> Allen Bool 
contains = hasConstraint Contains 

starts :: IntervalID -> IntervalID -> Allen Bool 
starts = hasConstraint Starts 

equals :: IntervalID -> IntervalID -> Allen Bool
equals = hasConstraint Equals 

startedBy :: IntervalID -> IntervalID -> Allen Bool 
startedBy = hasConstraint StartedBy 

during :: IntervalID -> IntervalID -> Allen Bool 
during = hasConstraint During 

finishes :: IntervalID -> IntervalID -> Allen Bool 
finishes = hasConstraint Finishes 

overlappedBy :: IntervalID -> IntervalID -> Allen Bool 
overlappedBy = hasConstraint OverlappedBy 

metBy :: IntervalID -> IntervalID -> Allen Bool 
metBy = hasConstraint MetBy 

precededBy :: IntervalID -> IntervalID -> Allen Bool 
precededBy = hasConstraint PrecededBy 
