module Data.Allen.Types ( Interval(..)
                        , Allen
                        , IntervalID
                        , IntervalGraph
                        , IntervalConstraint
                        , Relation(..)
                        , RelationBits
                        , allRelations
                        , toBits
                        , fromBits
                        , fromID
                        ) where  

import Control.Monad.State

import Data.Bits
import Data.List (intercalate)
import Data.Vector (Vector, (!))
import Data.Word (Word16)

type IntervalID = Int
type IntervalGraph = Vector Interval
type IntervalConstraint = (RelationBits, IntervalID)

data Interval = Interval { intervalID        :: Int 
                         , intervalRelations :: [IntervalConstraint]
                         } 

-- | Show instance for Interval 
-- Ex: Interval 3 (During 1, Contains 2)
instance Show Interval where 
    show (Interval iD rels) = "Interval " <> show iD <> " (" <> rels' <> ")"
        where rels' = intercalate ", " $ map showRel rels
              showRel (r, n) = unwords (map show $ fromBits r) <> " " <> show n

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

type RelationBits = Word16

allRelations :: [Relation]
allRelations  = [minBound..]

toBits :: Relation -> RelationBits
toBits r = 2 ^ fromEnum r

fromBits :: RelationBits -> [Relation]
fromBits bits = [x | x <- allRelations, bits .&. toBits x /= 0]
