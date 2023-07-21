module Main (main) where

import Data.Allen

calc :: Allen [Relation]
calc = do 
    eats   <- interval 
    sleeps <- interval 
    works  <- interval 

    assume eats Precedes sleeps 
    assume sleeps PrecededBy works 

    c1 <- getConstraints eats sleeps
    c2 <- getConstraints sleeps works

    let c3 = compose c1 c2

    assumeBits eats c3 works
    return $ fromBits c3

main :: IO ()
main = do 
    let relations = evalAllen calc

    putStrLn "Results: "
    putStrLn "-------------"
    mapM_ print relations

