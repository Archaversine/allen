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
    test prop_relationAdd

    putStrLn "\nTesting Intervals...\n"

    test prop_intervalAssume
    test prop_intervalAllBitsDefault

-- Throw error on test failure so that 
-- Spec.hs can properly recognize that a test has failed.
test :: Testable prop => prop -> IO ()
test func = do 
    result <- quickCheckWithResult stdArgs func 
    unless (isSuccess result) $ error "Test Failed!"

prop_relationBits :: ValidRelation -> Bool 
prop_relationBits (toRelation -> r) = r == head (fromBits $ toBits r)

prop_relationInverse :: ValidRelation -> Bool 
prop_relationInverse (toRelation -> r) = [r] == doubleInvert r
    where doubleInvert = fromBits . converse . converse . toBits

prop_relationBitAmount :: Bool 
prop_relationBitAmount = and $ zipWith (==) twos relations 
    where twos = map bit [0.. fromEnum (maxBound :: Relation)]
          relations = map toBits allRelations

prop_relationAdd :: ValidInterval -> ValidRelation -> IntervalID -> Bool 
prop_relationAdd (toInterval -> i) (toRelation -> toBits -> r) iD = newRelation == r
    where newInterval = addRelation i r iD
          newRelation = Map.findWithDefault 0 iD $ intervalRelations newInterval 

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

prop_intervalAllBitsDefault :: Bool 
prop_intervalAllBitsDefault = evalAllen calc
    where calc :: Allen Bool 
          calc = do 
            a <- interval 
            b <- interval 

            c1 <- getConstraints a b
            c2 <- getConstraints b a

            return (c1 == c2 && c1 == allRelationBits)
