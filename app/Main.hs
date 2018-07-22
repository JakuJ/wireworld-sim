module Main (main) where

import System.IO (stdin, hSetBuffering, BufferMode (LineBuffering, NoBuffering), stdout)

import Automaton (simulateAutomaton)
import Editing (modifyExistingAutomaton, createNewAutomaton)

-- |Actions that print the menu
menu :: [IO ()]
menu = [
    putStrLn "Menu:",
    putStrLn "1 - Load Grid from file",
    putStrLn "2 - Modify existing Grid",
    putStrLn "3 - Create new Grid",
    putStrLn "Other - Quit",
    putStr "Choice: " ]

-- |Program entry point
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    sequence_ menu
    hSetBuffering stdin NoBuffering
    choice <- getChar
    hSetBuffering stdin LineBuffering
    putStrLn "" -- newline after choice input
    case choice of
        '1' -> simulateAutomaton
        '2' -> modifyExistingAutomaton
        '3' -> createNewAutomaton
        _ -> putStrLn "Main thread terminated"