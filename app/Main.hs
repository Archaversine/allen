module Main (main) where

import Data.Allen

calc :: Allen ([Relation], [Relation]) 
calc = do 
    a <- interval 
    b <- interval 

    assume a Precedes b 

    r1 <- fromBits <$> getConstraints a b 
    r2 <- fromBits <$> getConstraints b a 

    return (r1, r2)

main :: IO ()
main = print $ evalAllen calc

