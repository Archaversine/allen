module Main (main) where

import Data.Allen

calc :: Allen [IntervalConstraint]
calc = do 
    sleeps <- interval 
    snores <- interval 

    assume snores During sleeps
    assume sleeps Starts snores

    -- Returns the relations between the sleeps and snores intervals
    getConstraints sleeps snores

main :: IO ()
main = do 
    let (relations, graph) = runAllenState calc 

    mapM_ print graph 

    putStrLn $ "Relations: " <> show relations
