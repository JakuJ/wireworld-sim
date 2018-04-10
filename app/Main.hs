module Main
( 
    main 
) where

import System.Console.ANSI
import System.IO            (stdin, hSetBuffering, hSetEcho, BufferMode (LineBuffering, NoBuffering))
import GHC.Conc             (ThreadStatus, ThreadId, threadStatus, threadDelay, killThread)
import Control.Concurrent   (MVar, newMVar, swapMVar, withMVar, modifyMVar_, forkFinally)
import Control.Monad        (forever, unless, when, replicateM)
import Control.Monad.Loops  (iterateWhile)
import Data.Maybe           (fromMaybe, fromJust, isJust, isNothing)

import Automaton
import Editing

simulateAutomaton :: IO () -- simulate a Wireworld automaton
simulateAutomaton = do
    -- * load Automaton file
    putStr "Enter Automaton save file path: "
    automatSav <- fmap (`Automaton` 0) $ loadGrid =<< getLine
    automatM <- newMVar automatSav
    -- * set console parameters
    hideCursor
    clearScreen
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    -- * spawn animation thread
    animationThread <- startAnimationThread automatM
    -- * animation interface (main thread)
    iterateWhile (==True) $ do -- TODO: Menu
        input <- getChar
        case input of
            'q' -> return False -- quit
            'r' -> do -- restart
                swapMVar automatM automatSav
                return True
            _ -> return True
    -- * set terminal parameters back to normal
    showCursor
    hSetBuffering stdin LineBuffering
    hSetEcho stdin True
    -- * print Automaton's period
    let period = calculatePeriod automatSav
    if isNothing period then putStrLn "No period" else putStrLn $ let (delay, time) = fromJust period in "Period of " ++ show time ++ " after " ++ show delay ++ " generations"
    -- * finish execution
    killThread animationThread -- ? unnecessary (main thread kills all children upon termination), but good practice
    threadDelay 1000 -- TODO: atomic putStrLn
    putStrLn "Simulation function ended"
        where
            delay = 500000 :: Int -- simulation's time step 
            startAnimationThread :: MVar Automaton -> IO ThreadId
            startAnimationThread automatM = flip forkFinally (\_ -> putStrLn "Animation thread terminated") $ forever $ do
                modifyMVar_ automatM $ \automat1 -> do
                    setCursorPosition 0 0 
                    (print . grid) automat1  
                    clearFromCursorToLineEnd 
                    putStrLn  ("Generation " ++ (show . generation) automat1) 
                    return $ Automaton ((nextGeneration . grid) automat1) (generation automat1 + 1) 
                threadDelay delay

main :: IO () -- program entry point
main = do
    -- * main menu
    hSetBuffering stdin NoBuffering
    putStrLn "Menu:"
    putStrLn "1 - Load Grid from file"
    putStrLn "2 - Create new Grid"
    putStrLn "Other - Quit"
    putStr "Choice: "
    choice <- getChar
    hSetBuffering stdin LineBuffering
    putStrLn ""
    case choice of
        '1' -> simulateAutomaton
        '2' -> createNewAutomaton
        _ -> putStrLn "Main thread terminated"