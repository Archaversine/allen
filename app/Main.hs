module Main (main) where

import Control.Monad
import Control.Monad.State

import Data.Allen

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import System.IO

data REPLState = REPLState { graph :: Allen (), intervalNames :: Map String IntervalID }
type REPL = StateT REPLState IO

newREPLState :: REPLState 
newREPLState = REPLState (return ()) Map.empty

createInterval :: String -> REPL ()
createInterval name = do 
    REPLState calc names <- get

    case Map.lookup name names of 
        Just _  -> error $ "Interval " ++ name ++ " already exists"
        Nothing -> do 
            let newID = Map.size names
            put $ REPLState (void $ calc >> interval) (Map.insert name newID names)

assumeRelation :: String -> RelationBits -> String -> REPL ()
assumeRelation a r b = do 
    REPLState calc names <- get

    case (Map.lookup a names, Map.lookup b names) of 
        (Just aID, Just bID) -> put $ REPLState (calc >> assumeBits aID r bID) names
        (Nothing, _)         -> error $ "Interval " <> a <> " does not exist"
        (_, Nothing)         -> error $ "Interval " <> b <> " does not exist"

showConstraints :: String -> String -> REPL ()
showConstraints a b = do 
    REPLState calc names <- get 

    case (Map.lookup a names, Map.lookup b names) of 
        (Just aID, Just bID) -> do 
            let constraints = fromBits $ evalAllen $ calc >> getConstraints aID bID
            liftIO $ putStrLn $ a <> " --(" <> map relationToChar constraints <> ")--> " <> b
        (Nothing, _) -> error $ "Interval " <> a <> " does not exist"
        (_, Nothing) -> error $ "Interval " <> b <> " does not exist"

showGraph :: REPL ()
showGraph = do 
    calc <- gets graph
    liftIO $ mapM_ (print . snd) $ Map.toList $ execAllen calc

resetGraph :: REPL ()
resetGraph = put newREPLState

data Command = CreateInterval String 
             | AssumeRelation String RelationBits String
             | GetConstraints String String
             | ShowGraph
             | ResetGraph
             | Quit
             | InvalidCommand

parseCommand :: String -> Command 
parseCommand str = case words str of 
    [ "create", name      ] -> CreateInterval name 
    [ "assume", a, rel, b ] -> AssumeRelation a (bitsFromString rel) b
    [ "constraints", a, b ] -> GetConstraints a b 
    [ "graph"             ] -> ShowGraph 
    [ "clear"             ] -> ResetGraph 
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

repl :: REPL () 
repl = do 
    command <- liftIO readCommand

    case command of 
        CreateInterval name  -> createInterval name >> repl
        AssumeRelation a r b -> assumeRelation a r b >> repl
        GetConstraints a b   -> showConstraints a b >> repl
        ShowGraph            -> showGraph >> repl
        ResetGraph           -> resetGraph >> repl
        Quit                 -> return ()
        InvalidCommand       -> do 
            liftIO $ putStrLn "Error: Invalid Command"
            repl

main :: IO ()
main = do 
    -- Fix buffering so that we can see the prompt
    hSetBuffering stdout NoBuffering
    evalStateT repl newREPLState

