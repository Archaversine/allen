module Main (main) where

import Data.Allen

import System.IO

data Command = CreateInterval String 
             | AssumeRelation String String RelationBits
             | GetConstraints String String
             | ShowGraph
             | ResetGraph
             | Quit
             | InvalidCommand

parseCommand :: String -> Command 
parseCommand str = case words str of 
    [ "create", name      ] -> CreateInterval name 
    [ "assume", a, rel, b ] -> AssumeRelation a b (bitsFromString rel)
    [ "constraints", a, b ] -> GetConstraints a b 
    [ "graph"             ] -> ShowGraph 
    [ "clear"             ] -> ResetGraph 
    [ "quit"              ] -> Quit 
    [ "exit"              ] -> Quit
    _                       -> InvalidCommand

readCommand :: IO Command 
readCommand = do 
    putStr "Allen > "
    command <- parseCommand <$> getLine

    case command of 
        InvalidCommand -> do 
            putStrLn "Invalid command"
            readCommand
        _ -> return command

main :: IO ()
main = do 
    -- Fix buffering so that we can see the prompt
    hSetBuffering stdout NoBuffering

