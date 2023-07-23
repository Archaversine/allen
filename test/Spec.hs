{-# LANGUAGE ViewPatterns #-}

import Test.QuickCheck

import Control.Monad (unless)

import Data.Allen

-- To Avoid orphaned instance warning
newtype ValidRelation = ValidRelation { toRelation :: Relation }

instance Arbitrary ValidRelation where 
    arbitrary = do 
        index <- arbitrary 
        let n = fromEnum (maxBound :: Relation)

        return $ ValidRelation $ toEnum $ index `mod` n

instance Show ValidRelation where 
    show = show . toRelation

main :: IO ()
main = do 
    putStrLn "Testing Relations...\n"

    test prop_relationBits
    test prop_relationInverse
    test prop_relationBitAmount

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
    where twos = map (2^) [0.. fromEnum (maxBound :: Relation)]
          relations = map toBits allRelations
