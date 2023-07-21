module Main (main) where

import Data.Allen

calc :: Allen [Relation]
calc = do 
    eats   <- interval 
    sleeps <- interval 
    works  <- interval 

    assume eats Precedes sleeps 
    assume sleeps PrecededBy works 

    return $ fromBits $ Precedes `compose` PrecededBy

main :: IO ()
main = putStrLn $ "Results: " <> show (evalAllen calc)

