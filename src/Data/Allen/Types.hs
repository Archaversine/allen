module Data.Allen.Types ( Interval(..)
                        , Allen
                        , IntervalGraph
                        , Relation(..)
                        , IntervalConstraint
                        ) where  

import Control.Monad.State

import Data.List (intercalate)
import Data.Vector (Vector)

type IntervalGraph = Vector Interval
type IntervalConstraint = (Relation, Interval)

data Interval = Interval { intervalID        :: Int 
                         , intervalRelations :: [IntervalConstraint]
                         } deriving Eq

-- | Show instance for Interval 
-- Ex: Interval 3 (During 1, Contains 2)
instance Show Interval where 
    show (Interval iD rels) = "Interval " <> show iD <> " (" <> rels' <> ")"
        where rels' = intercalate ", " $ map showRel rels
              showRel (r, Interval rID _) = show r <> " " <> show rID

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
