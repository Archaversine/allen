module Data.Allen.Types ( Interval(..)
                        , Allen
                        , IntervalID
                        , IntervalGraph
                        , IntervalConstraint
                        , Relation(..)
                        , fromID
                        ) where  

import Control.Monad.State

import Data.List (intercalate)
import Data.Vector (Vector, (!))

type IntervalID = Int
type IntervalGraph = Vector Interval
type IntervalConstraint = (Relation, IntervalID)

data Interval = Interval { intervalID        :: Int 
                         , intervalRelations :: [IntervalConstraint]
                         } 

-- | Show instance for Interval 
-- Ex: Interval 3 (During 1, Contains 2)
instance Show Interval where 
    show (Interval iD rels) = "Interval " <> show iD <> " (" <> rels' <> ")"
        where rels' = intercalate ", " $ map showRel rels
              showRel (r, n) = show r <> " " <> show n

fromID :: IntervalID -> Allen Interval 
fromID n = gets (! n)

type Allen = State IntervalGraph

data Relation = Precedes 
              | Meets 
              | Overlaps 
              | FinishedBy
              | Contains 
              | Starts 
              | Equals 
              | StartedBy 
              | During 
              | Finishes 
              | OverlappedBy 
              | MetBy
              | PrecededBy
              deriving (Eq, Show, Enum, Bounded)
