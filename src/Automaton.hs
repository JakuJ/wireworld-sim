module Automaton
(
    State, Cell (..), Grid (..), Automaton (..),
    loadGrid, saveGrid, 
    fillGrid,
    simulateAutomaton
) where

import System.Console.ANSI
import System.IO            (stdin, hSetBuffering, hSetEcho, BufferMode (LineBuffering, NoBuffering))
import Data.Tuple           (swap)
import Data.List            (find)
import Data.List.Split      (chunksOf)
import GHC.Conc             (ThreadId, threadDelay, killThread)
import Control.Concurrent   (MVar, newMVar, swapMVar, modifyMVar_, forkFinally)
import Control.Monad        (forever)
import Control.Monad.Loops  (iterateWhile)
import Data.Maybe           (fromMaybe, fromJust, isNothing)

-- |A datatype for different statuses that an automaton's cell can have
data State = Empty | Wire | Head | Tail
    deriving (Eq, Enum)

-- |A datatype for automaton's cells
data Cell = Cell {
    state :: State,
    row :: Int,
    column :: Int
} deriving (Eq, Show)

-- |A datatype for whole grids of cells
data Grid = Grid {
    cells :: [Cell],
    rows :: Int,
    columns :: Int
} deriving (Eq)

-- |A datatype for a whole automaton
data Automaton = Automaton {
    grid :: Grid,
    generation :: Int
}

instance Show State where 
    show state = fromJust $ lookup (stateToChar state) colors

instance Show Grid where
    show grid = concatMap (\c -> fromMaybe "\n" (lookup c colors)) (gridToString grid)

-- |A dictionary used in printing to console
colors :: [(Char, String)]
colors = [('.', " "), ('#', "\x1b[43m \x1b[0m"), ('X', "\x1b[46m \x1b[0m"), ('O', "\x1b[41m \x1b[0m")]

-- |A dictionary used to encode cell states as chars
states :: [(State, Char)] 
states = [(Empty, '.'), (Wire, '#'), (Head, 'X'), (Tail, 'O')]

-- |Decode cell state from char
charToState :: Char -> State
charToState c = fromMaybe Empty $ lookup c $ map swap states

-- |Encode cell state as char
stateToChar :: State -> Char 
stateToChar s = fromMaybe '.' $ lookup s states

-- |Encode entire grid as a string !deprecated!
gridToString :: Grid -> String 
gridToString (Grid cells m n) = unlines $ map (map (stateToChar . state)) $ chunksOf n cells

-- |Save encoded grid to file
saveGrid :: Grid -> FilePath -> IO ()
saveGrid g path = writeFile path (gridToString g)

-- |Load encoded grid from file
loadGrid :: FilePath -> IO Grid
loadGrid path = do
    stringRep <- readFile path
    let rows = lines stringRep
    let m = length rows
    let n = length (head rows)
    let cells = [Cell (charToState ((rows!!x)!!y) ) x y | x <- [0 .. m-1], y <- [0 .. n-1]]
    let grid = Grid cells m n
    return grid

-- |Fills Grid which only has non-Empty cells to m x n size with Empty cells
fillGrid :: Grid -> Grid 
fillGrid (Grid cells ms ns) = Grid cells' ms ns
    where
        cells' = map (\(x, y) -> (fromMaybe (Cell Empty x y) (found x y))) $ (,) <$> [0 .. ms - 1] <*> [0 .. ns - 1]
        found x y = find (\(Cell s m n) -> m == x && n == y) cells

-- |Get cell and its Moore neighbourhood in a list
getNeighbours :: Grid -> Cell -> [Cell]
getNeighbours grid cell = [cells grid!!(columns grid * m + n) | m <- ms, n <- ns]
    where
        x = row cell
        y = column cell
        ms = filter (\z -> z >= 0 && z < rows grid) [x - 1, x, x + 1]
        ns = filter (\z -> z >= 0 && z < columns grid) [y - 1, y, y + 1]

-- |Generate next generation of an Automaton
nextGeneration :: Grid -> Grid
nextGeneration g@(Grid cs m n) = Grid (map ruleset cs) m n
    where
        heads c = length $ filter (\(Cell state _ _) -> state == Head) (getNeighbours g c) -- count electron heads in cell's neighbourhood
        -- |Determines how cells change in time
        ruleset :: Cell -> Cell 
        ruleset c@(Cell s x y) 
            | s == Empty = c
            | s == Head = Cell Tail x y
            | s == Tail = Cell Wire x y
            | s == Wire = if heads c == 1 || heads c == 2 then Cell Head x y else Cell Wire x y

-- |If the automaton loops before 500 iterations, return period of this loop            
calculatePeriod :: Automaton -> Maybe (Int, Int)
calculatePeriod automat = recursiveCheck (grid automat) []
    where
        -- |Takes current grid and a list of all previous grids
        recursiveCheck :: Grid -> [Grid] -> Maybe (Int, Int) 
        recursiveCheck grid gs
            | length gs >= 500 = Nothing
            | grid `elem` gs = Just $ (,) (length (takeWhile (/=grid) gs)) $ (+) 1 $ length $ takeWhile (/= grid) $ tail $ dropWhile (/= grid) gs
            | otherwise = recursiveCheck (nextGeneration grid) $ gs ++ [grid]

-- |Simulate a Wireworld automaton            
simulateAutomaton :: IO () 
simulateAutomaton = do
    -- load Automaton file
    putStr "Enter Automaton save file path: "
    automatSav <- fmap (`Automaton` 0) $ loadGrid =<< getLine
    automatM <- newMVar automatSav
    -- set console parameters
    hideCursor
    clearScreen
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    -- spawn animation thread
    animationThread <- startAnimationThread automatM
    -- animation interface (main thread)
    iterateWhile (==True) $ do -- TODO: Menu
        input <- getChar
        case input of
            'q' -> return False -- quit
            'r' -> do -- restart
                swapMVar automatM automatSav
                return True
            _ -> return True
    -- set terminal parameters back to normal
    showCursor
    hSetBuffering stdin LineBuffering
    hSetEcho stdin True
    -- print Automaton's period
    let period = calculatePeriod automatSav
    if isNothing period then putStrLn "No period" else putStrLn $ let (delay, time) = fromJust period in "Period of " ++ show time ++ " after " ++ show delay ++ " generations"
    -- finish execution
    killThread animationThread -- ? unnecessary, but it's good practice to remember your children
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
                    putStrLn "Press Q to quit"
                    return $ Automaton ((nextGeneration . grid) automat1) (generation automat1 + 1) 
                threadDelay delay
            