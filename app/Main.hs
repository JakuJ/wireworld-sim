module Main where

import System.IO (stdin, hSetBuffering, hSetEcho, BufferMode (LineBuffering, NoBuffering))
import System.Console.ANSI -- too many functions to list them all
import GHC.Conc (ThreadStatus, ThreadId, threadStatus, threadDelay, killThread)
import Control.Concurrent (MVar, newMVar, swapMVar, modifyMVar_, forkFinally)
import Control.Monad (forever, unless)
import Control.Monad.Loops (iterateWhile)
import Data.Tuple (swap)
import Data.List (find, delete)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, fromJust, isJust, isNothing)
import Data.Char (digitToInt)

data State = Empty | Wire | Head | Tail deriving (Eq, Enum)

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

instance Show State where -- overloaded Show instance for type State
    show state = fromJust $ lookup (stateToChar state) colors

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

getChar' :: IO Char -- getChar without echo
getChar' = do 
    hSetEcho stdin False
    input <- getChar
    hSetEcho stdin True
    return input

fillGrid :: Grid -> Grid -- fills Grid which only has non-Empty cells to m x n size with Empty cells
fillGrid (Grid cells ms ns) = Grid (map (\(x, y) -> (fromMaybe (Cell Empty x y) (found x y))) (cart [0 .. ms - 1] [0 .. ns - 1])) ms ns
    where
        found x y = find (\(Cell s m n) -> m == x && n == y) cells
        cart :: [a] -> [b] -> [(a, b)] -- Cartesian product of two vectors
        cart xs ys = [(x, y) | x <- xs, y <- ys]

createNewAutomaton :: IO ()
createNewAutomaton = do
    putStrLn "Enter board size (m n): "
    dims <- map (\x -> read x :: Int) . words <$> getLine
    let [m, n] = dims
    drawBorder m n
    putStrLn "WSAD - move, 0 - empty, 1 - wire, 2 - e. head, 3 - e. tail, 9 - finish"
    -- * main logic
    setCursorPosition 1 1
    newGrid <- fillGrid <$> modifyGrid (Grid [] m n)
    putStrLn "Enter new Automaton's save path (empty for no save): "
    input <- getLine
    unless (null input) $ saveGrid newGrid input
    putStrLn "Creating new automaton successfull!"
    where
        drawBorder :: Int -> Int -> IO () -- draws a border of size m x n
        drawBorder m n = do
            clearScreen
            setCursorPosition 0 0
            putStrLn $ replicate (n + 2) '-'
            mapM_ (\_ -> putStr "|" >> cursorForward n >> putStrLn "|") [1 .. m]
            putStrLn $ replicate (n + 2) '-'
        movements :: [(Char, IO ())] -- movement functions lookup table
        movements = [('w', cursorUp 1), ('s', cursorDown 1), ('a', cursorBackward 1), ('d', cursorForward 1)]
        move :: Char -> Int -> Int -> IO () -- move cursor in terminal using wsad, can't go over the border
        move c m n = do
            pos <- fromMaybe (0, 0) <$> getCursorPosition
            if (fst pos == 2 && c == 'w') || (fst pos == m + 1 && c == 's') || (snd pos == 2 && c == 'a') || (snd pos == n + 1 && c == 'd') 
                then putStr ""
            else fromJust $ lookup c movements
        modifyGrid :: Grid -> IO Grid -- create new Grid by adding cells using accumulative recursion
        modifyGrid grid = do
            input <- getChar'
            if input `elem` "wsad" then move input m n >> modifyGrid grid
            else case input of
                x | x `elem` "0123" -> do
                    pos <- fromJust <$> getCursorPosition
                    let chosenState = stateMap !! digitToInt x
                    (putStr . show) chosenState
                    cursorBackward 1
                    modifyGrid (Grid (cells grid ++ [uncurry (Cell chosenState) ((\(x,y) -> (x-2, y-2)) pos)]) m n)
                '9' -> do
                    setCursorPosition (m + 3)  0
                    if null (cells grid) 
                        then return $ Grid [] 0 0
                    else return $ Grid (foldl (\c1 c2 -> if row (last c1) == row c2 && column (last c1) == column c2 then init c1 ++ [c2] else c1 ++ [c2]) [head (cells grid)] (tail (cells grid))) m n
                _ -> modifyGrid grid
            where
                m = rows grid
                n = columns grid
                stateMap = enumFrom (toEnum 0) :: [State]

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
    if isNothing period then putStrLn "No period" else putStrLn $ "Period of " ++ show (fromJust period)
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
    putStrLn "Menu:"
    putStrLn "1 - Load Grid from file"
    putStrLn "2 - Create new Grid"
    putStrLn "Other - Quit"
    putStr "Choice: "
    choice <- getChar
    putStrLn ""
    case choice of
        '1' -> simulateAutomaton
        '2' -> createNewAutomaton
        _ -> putStrLn "Main thread terminated"