{-# LANGUAGE BinaryLiterals #-}

module Main (main) where 

import Criterion.Main
import Criterion.Types

import Data.Allen

benchConfig :: Config 
benchConfig = defaultConfig { reportFile = Just "benchmark.html" }

main :: IO ()
main = defaultMainWith benchConfig [
  bgroup "Relation" [ bench "compose-0bits " $ whnf testCompose 0b0000000000000000
                    , bench "compose-1bit  " $ whnf testCompose 0b0000000000000001
                    , bench "compose-2bits " $ whnf testCompose 0b0000000000000011
                    , bench "compose-4bits " $ whnf testCompose 0b0000000000001111
                    , bench "compose-8bits " $ whnf testCompose 0b0000000011111111
                    , bench "compose-13bits" $ whnf testCompose 0b0001111111111111
                    ]
  ]

testCompose :: RelationBits -> RelationBits
testCompose x = compose x x
