{-# LANGUAGE ViewPatterns #-}

import Test.QuickCheck

import Control.Monad (unless)

import Data.Allen
import Data.Bits

import qualified Data.Map as Map

-- To Avoid orphaned instance warning
newtype ValidRelation = ValidRelation { toRelation :: Relation }
newtype ValidInterval = ValidInterval { toInterval :: Interval }

instance Arbitrary ValidRelation where 
    arbitrary = do 
        index <- arbitrary 
        let n = fromEnum (maxBound :: Relation)

        return $ ValidRelation $ toEnum $ index `mod` n

instance Arbitrary ValidInterval where 
    arbitrary = do 
        iD <- abs <$> arbitrary 
        return $ ValidInterval $ Interval iD Map.empty

instance Show ValidRelation where 
    show = show . toRelation

instance Show ValidInterval where 
    show = show . toInterval

main :: IO ()
main = do 
    putStrLn "Testing Relations...\n"

    test prop_relationBits
    test prop_relationInverse
    test prop_relationBitAmount
    test prop_relationSet
    test prop_relationTestSet

    putStrLn "\nTesting Intervals...\n"

    test prop_intervalAssume
    testAllen prop_intervalAllBitsDefault

    putStrLn "\nTests from Allen (1983)...\n"

    testAllen prop_Section4Subsection2Part1
    testAllen prop_Section4Subsection2Part2
    testAllen prop_Section4Subsection2Part3


-- Throw error on test failure so that 
-- Spec.hs can properly recognize that a test has failed.
test :: Testable prop => prop -> IO ()
test func = do 
    result <- quickCheckWithResult stdArgs func 
    unless (isSuccess result) $ error "Test Failed!"

-- Similar to the `test` function but for Allen calculations
-- Upon failure: throws error and prints results of the calculation
testAllen :: Allen Bool -> IO ()
testAllen calc = do 
    let (successful, result) = runAllen calc
    if successful then 
        putStrLn "+++ OK, passed 1 test."
    else do
        putStrLn "--- [ Test Failed! ] ---"
        putStrLn "Result: "
        putStrLn "------------------------"
        mapM_ (print . snd) $ Map.toList result
        putStrLn "------------------------"
        errorWithoutStackTrace "Test Failure."

prop_relationBits :: ValidRelation -> Bool 
prop_relationBits (toRelation -> r) = r == head (fromBits $ toBits r)

prop_relationInverse :: ValidRelation -> Bool 
prop_relationInverse (toRelation -> toBits -> r) = r == doubleConverse r
    where doubleConverse = converse . converse

prop_relationBitAmount :: Bool 
prop_relationBitAmount = and $ zipWith (==) twos relations 
    where twos = map bit [0.. fromEnum (maxBound :: Relation)]
          relations = map toBits allRelations

prop_relationSet :: ValidInterval -> ValidRelation -> ValidRelation -> IntervalID -> Bool 
prop_relationSet (toInterval -> i) (toRelation -> toBits -> r1) (toRelation -> toBits -> r2) iD = r == r2
    where i'  = setRelation i r1 iD
          i'' = setRelation i' r2 iD 
          r   = intervalRelations i'' Map.! iD

prop_relationTestSet :: [ValidRelation] -> Bool 
prop_relationTestSet (map toRelation -> r) = evalAllen calc 
    where calc :: Allen Bool 
          calc = do 
            a <- interval 
            b <- interval 

            assumeBits a allRelationBits b
            testRelationSet r a b

prop_intervalAssume :: ValidRelation -> Bool 
prop_intervalAssume (toRelation -> r) = evalAllen calc 
    where calc :: Allen Bool
          calc = do 
            a <- interval 
            b <- interval 

            assume a r b

            r1 <- getConstraints a b
            r2 <- getConstraints b a

            return $ r1 == converse r2

prop_intervalAllBitsDefault :: Allen Bool 
prop_intervalAllBitsDefault = do
    a <- interval 
    b <- interval 

    c1 <- getConstraints a b
    c2 <- getConstraints b a

    return (c1 == c2 && c1 == allRelationBits)

--
-- Examples from Section 4.2 of the Allen (1983) paper.
--

networkSection4Subsection2 :: Allen (IntervalID, IntervalID, IntervalID)
networkSection4Subsection2 = do 
    r <- interval 
    s <- interval 
    l <- interval

    assumeSet s [Precedes, Meets, MetBy, PrecededBy] r
    assumeSet s [Overlaps, Meets] l

    return (r, s, l)

-- First inference from Section 4.2
prop_Section4Subsection2Part1 :: Allen Bool
prop_Section4Subsection2Part1 = do 
    -- Set up basic network.
    -- s is discarded since it is not used
    (r, _, l) <- networkSection4Subsection2

    lrInferredBits <- getConstraints l r
    -- expected [Precedes, PrecededBy, Overlaps, Meets, Contains, Starts, StartedBy, FinishedBy, Equals]
    let lrExpectedBits = bitsFromString "pPomDsSFe"

    return (lrInferredBits == lrExpectedBits)

-- Additional inference from Section 4.2.
-- Assumes part 1 inference works.
prop_Section4Subsection2Part2 :: Allen Bool
prop_Section4Subsection2Part2 = do
    -- Set up same network as Part 1, but with added l->r relations.
    (r, s, l) <- networkSection4Subsection2

    assumeSet l [Overlaps, Starts, During] r

    lrInferredBits <- getConstraints l r
    srInferredBits <- getConstraints s r
    -- lrExpected [Overlaps, Starts]
    -- srExpected [Precedes, Meets]
    let lrExpectedBits = bitsFromString "os"
        srExpectedBits = bitsFromString "pm"

    return (lrInferredBits == lrExpectedBits
         && srInferredBits == srExpectedBits)

-- Final inference from Section 4.2.
-- Assumes inferences from parts 1 and 2 works.
prop_Section4Subsection2Part3 :: Allen Bool
prop_Section4Subsection2Part3 = do
    -- Set up same network as Part 2.
    (r, s, l) <- networkSection4Subsection2

    assumeSet l [Overlaps, Starts, During] r

    -- Add new interval D
    -- w/ D -[During]-> S
    d <- interval
    let dsRelationBits = bitsFromString "d"
    assumeBits d dsRelationBits s

    drInferredBits <- getConstraints d r
    dlInferredBits <- getConstraints d l
    -- drExpected [Precedes]
    -- dlExpected [Precedes, Overlaps, Meets, During, Starts]
    let drExpectedBits = bitsFromString "p"
        dlExpectedBits = bitsFromString "pomds"

    return (drInferredBits == drExpectedBits
         && dlInferredBits == dlExpectedBits)

