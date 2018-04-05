module Main where

import System.IO (stdin, hSetBuffering, hSetEcho, BufferMode (LineBuffering, NoBuffering))
import System.Console.ANSI (hideCursor, showCursor, clearScreen, setCursorPosition, clearFromCursorToLineEnd)
import GHC.Conc (ThreadStatus, ThreadId, threadStatus, threadDelay, killThread)
import Control.Concurrent (MVar, newMVar, swapMVar, modifyMVar_, forkFinally)
import Control.Monad (forever)
import Control.Monad.Loops (iterateWhile)
import Data.Tuple (swap)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)

data State = Empty | Wire | Head | Tail deriving (Show, Eq)

data Cell = Cell {
    state :: State,
    row :: Int,
    column :: Int
} deriving (Eq, Show)

data Grid = Grid {
    cells :: [Cell],
    rows :: Int,
    columns :: Int
} deriving (Eq)

data Automaton = Automaton {
    grid :: Grid,
    generation :: Int
}

instance Show Grid where -- overloaded Show instance for type Grid
    show grid = concatMap (\c -> fromMaybe "\n" (lookup c colors)) (gridToString grid)

colors :: [(Char, String)] -- a dictionary used in printing to console
colors = [('.', " "), ('#', "\x1b[43m \x1b[0m"), ('X', "\x1b[46m \x1b[0m"), ('O', "\x1b[41m \x1b[0m")]

states :: [(State, Char)] -- a dictionary used to encode cell states as chars
states = [(Empty, '.'), (Wire, '#'), (Head, 'X'), (Tail, 'O')]

charToState :: Char -> State -- decode cell state from char
charToState c = fromMaybe Empty $ lookup c $ map swap states

stateToChar :: State -> Char -- encode cell state as char
stateToChar s = fromMaybe '.' $ lookup s states

gridToString :: Grid -> String -- encode entire grid as a string -- ? Probably unnecessary
gridToString (Grid cells m n) = unlines $ map (map (stateToChar . state)) $ chunksOf n cells

saveGrid :: Grid -> FilePath -> IO () -- save encoded grid to file
saveGrid g path = writeFile path (gridToString g)

loadGrid :: FilePath -> IO Grid -- load encoded grid from file
loadGrid path = do
    stringRep <- readFile path
    let rows = lines stringRep
    let m = length rows
    let n = length (head rows)
    let cells = [Cell (charToState ((rows!!x)!!y) ) x y | x <- [0 .. m-1], y <- [0 .. n-1]]
    let grid = Grid cells m n
    return grid

initialize :: Int -> Int -> Grid -- create new empty m * n grid
initialize m n = Grid [Cell Empty x y | x <- [0 .. m - 1], y <- [0 .. n - 1]] m n

getNeighbours :: Grid -> Cell -> [Cell] -- get [cell] ++ its Moore neighbourhood
getNeighbours grid cell = [cells grid!!(columns grid * m + n) | m <- ms, n <- ns]
    where
        x = row cell
        y = column cell
        ms = filter (\z -> z >= 0 && z < rows grid) [x - 1, x, x + 1]
        ns = filter (\z -> z >= 0 && z < columns grid) [y - 1, y, y + 1]

nextGeneration :: Grid -> Grid -- generate next generation of an Automaton
nextGeneration g@(Grid cs m n) = Grid (map ruleset cs) m n
    where
        heads c = length $ filter (\(Cell state _ _) -> state == Head) (getNeighbours g c) -- count electron heads in cell's neighbourhood
        ruleset :: Cell -> Cell -- determines how cells change in time
        ruleset c@(Cell s x y) 
            | s == Empty = c
            | s == Head = Cell Tail x y
            | s == Tail = Cell Wire x y
            | s == Wire = if heads c == 1 || heads c == 2 then Cell Head x y else Cell Wire x y

calculatePeriod :: Automaton -> Maybe Int -- if the automaton loops before 500 iterations, return period of this loop
calculatePeriod automat = recursiveCheck (grid automat) []
    where
        recursiveCheck :: Grid -> [Grid] -> Maybe Int -- takes current grid and a list of all previous grids
        recursiveCheck grid gs
            | length gs >= 500 = Nothing
            | grid `elem` gs = Just $ (+) 1 $ length $ takeWhile (/= grid) $ tail $ dropWhile (/= grid) gs
            | otherwise = recursiveCheck (nextGeneration grid) $ gs ++ [grid]

main :: IO () -- main program function
main = do
    -- * load Automaton file
    putStrLn "Enter Automaton save file path: "
    automatSav <- fmap (`Automaton` 0) $ loadGrid =<< getLine
    automatM <- newMVar automatSav
    -- * set console parameters
    hideCursor
    clearScreen
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    -- * spawn animation thread
    animationThread <- startAnimationThread automatM
    -- * interface (main thread)
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
    let period = fromMaybe (-1) $ calculatePeriod automatSav
    if period < 0 then putStrLn "No period" else putStrLn $ "Period of " ++ show period
    -- * finish execution
    killThread animationThread -- ? unnecessary (main thread kills all children upon termination), but good practice
    threadDelay 1000 -- TODO: atomic putStrLn
    putStrLn "Main thread terminated" 
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