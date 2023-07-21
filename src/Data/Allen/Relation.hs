{-# LANGUAGE ViewPatterns #-}

module Data.Allen.Relation ( inverse
                           , hasRelation
                           , relationUnion
                           , composeSingle
                           , compose
                           , bitsFromString
                           ) where

import Data.Allen.Types
import Data.Bits

import qualified Data.Vector.Unboxed as U

-- | Lookup table for inverse function
inverseLookup :: [(RelationBits, RelationBits)]
inverseLookup = zip bits (reverse bits)
    where bits = map toBits allRelations

inverse :: RelationBits -> RelationBits
inverse r = snd $ head $ filter ((== r) . fst) inverseLookup

hasRelation :: Relation -> IntervalID -> IntervalID -> Allen Bool
hasRelation (toBits -> r) (fromID -> a) b = do 
    let match (bits, i) | i == b = (r .&. bits) /= 0 
                        | otherwise = False

    any match . intervalRelations <$> a

relationFromChar :: Char -> Relation
relationFromChar x = case x of 
    'p' -> Precedes 
    'm' -> Meets 
    'o' -> Overlaps 
    'F' -> FinishedBy 
    'D' -> Contains 
    's' -> Starts 
    'e' -> Equals 
    'S' -> StartedBy 
    'd' -> During 
    'f' -> Finishes 
    'O' -> OverlappedBy 
    'M' -> MetBy 
    'P' -> PrecededBy 
    _   -> error $ "relationFromChar: invalid relation " <> [x]

relationUnion :: [RelationBits] -> RelationBits
relationUnion = foldl (.|.) 0

bitsFromString :: String -> RelationBits
bitsFromString x | x == "full"   = rBits allRelations 
                 | x == "concur" = rBits [Overlaps .. OverlappedBy]
                 | otherwise = rBits $ map relationFromChar x
    where rBits = relationUnion . map toBits

-- Table referenced from here: https://www.ics.uci.edu/~alspaugh/cls/shr/allen.html
composeLookup :: U.Vector RelationBits
composeLookup = U.fromList $ map bitsFromString table 
--                |    p   |    m   |   o     |  F     |  D     |  s     |  e |   S    |     d   |    f   |     O   |    M   |    P
-- ---------------+--------+--------+---------+--------+--------+--------+----+--------+---------+--------+---------+--------+-------------
    where table = [     "p",     "p",      "p",     "p",     "p",     "p", "p",     "p",  "pmosd", "pmosd",  "pmosd", "pmosd",  "full" -- p
                  ,     "p",     "p",      "p",     "p",     "p",     "m", "m",     "m",    "osd",   "osd",    "osd",   "Fef", "DSOMP" -- m
                  ,     "p",     "p",    "pmo",   "pmo", "pmoFD",     "o", "o",   "oFD",    "osd",   "osd", "concur",   "DSO", "DSOMP" -- o
                  ,     "p",     "m",      "o",     "F",     "D",     "o", "F",     "D",    "osd",   "Fef",    "DSO",   "DSO", "DSOMP" -- F
                  , "pmoFD",   "oFD",    "oFD",     "D",     "D",   "oFD", "D",     "D", "concur",   "DSO",    "DSO",   "DSO", "DSOMP" -- D
                  ,     "p",     "p",    "pmo",   "pmo", "pmoFD",     "s", "s",   "seS",      "d",     "d",    "dfO",     "M",     "P" -- s
                  ,     "p",     "m",      "o",     "F",     "D",     "s", "e",     "S",      "d",     "f",      "O",     "M",     "P" -- e
                  , "pmoFD",   "oFD",    "oFD",     "D",     "D",   "seS", "S",     "S",    "dfO",     "O",      "O",     "M",     "P" -- S
                  ,     "p",     "p",  "pmosd", "pmosd",  "full",     "d", "d", "dfOMP",      "d",     "d",  "dfOMP",     "P",     "P" -- d
                  ,     "p",     "m",    "osd",   "Fef", "DSOMP",     "d", "f",   "OMP",      "d",     "f",    "OMP",     "P",     "P" -- f
                  , "pmoFD",   "oFD", "concur",   "DSO", "DSOMP",   "dfO", "O",   "OMP",    "dfO",     "O",    "OMP",     "P",     "P" -- O
                  , "pmoFD",   "seS",    "dfO",     "M",     "P",   "dfO", "M",     "P",    "dfO",     "M",      "P",     "P",     "P" -- M
                  ,  "full", "dfOMP", "dfOMOP",     "P",     "P", "dfOMP", "P",     "P",  "dfOMP",     "P",      "P",     "P",     "P" -- P
                  ]

composeSingle :: Relation -> Relation -> RelationBits 
composeSingle (fromEnum -> r1) (fromEnum -> r2) = composeLookup U.! index
    where index = 13 * r1 + r2

-- TODO: Verify correctness of this function
compose :: RelationBits -> RelationBits -> RelationBits
compose (fromBits -> r1) (fromBits -> r2) = relationUnion [composeSingle a b | a <- r1, b <- r2]
