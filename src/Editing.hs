module Editing
(
    createNewAutomaton,
    modifyExistingAutomaton
) where

import System.Console.ANSI
import System.IO            (stdin, hSetBuffering, hSetEcho, BufferMode (LineBuffering, NoBuffering))
import Data.Char            (digitToInt)

import Automaton

getChar' :: IO Char -- getChar without echo
getChar' = do 
    hSetEcho stdin False
    input <- getChar
    hSetEcho stdin True
    return input

pprint :: Show a => [a] -> IO () -- pretty prints lists of lists
pprint = mapM_ print

-- TODO: update to Functor version
print2D :: Grid -> (Int, Int) -> IO ()
print2D grid (x, y) = mapM_ (\cell -> setCursorPosition (row cell + x) (column cell + y) >> print (state cell)) $ cells grid

drawBorder :: Int -> Int -> IO () -- draws a border of size m x n
drawBorder m n = do
    clearScreen
    setCursorPosition 0 0
    putStrLn $ replicate (n + 2) '-'
    mapM_ (\_ -> putStr "|" >> cursorForward n >> putStrLn "|") [1 .. m]
    putStrLn $ replicate (n + 2) '-'

move :: Char -> (Int, Int) -> (Int, Int) -> IO (Int, Int) -- move cursor in terminal using wsad, can't go over the border
move c (m, n) pos@(x, y) =
    if (fst pos == 0 && c == 'w') || (fst pos == m - 1 && c == 's') || (snd pos == 0 && c == 'a') || (snd pos == n - 1 && c == 'd') 
        then return pos
    else case c of
        'w' -> cursorUp 1 >> return (x - 1, y)
        's' -> cursorDown 1 >> return (x + 1, y)
        'a' -> cursorBackward 1 >> return (x, y - 1)
        'd' -> cursorForward 1 >> return (x, y + 1)

editionLoop :: Grid -> (Int, Int) -> IO Grid -- create new Grid by adding cells using accumulative recursion
editionLoop grid pos = do
    input <- getChar'
    if input `elem` "wsad" then editionLoop grid =<< move input (m, n) pos
    else case input of
        x | x `elem` "0123" -> do
            let chosenState = stateList !! digitToInt x
            (putStr . show) chosenState
            cursorBackward 1
            editionLoop (Grid (addNewer (cells grid) (uncurry (Cell chosenState) pos)) m n) pos
        '9' -> do
            setCursorPosition (m + 3)  0
            if null (cells grid) 
                then return $ Grid [] 0 0
            else return grid
        _ -> editionLoop grid pos
    where
        m = rows grid
        n = columns grid
        stateList = enumFrom (toEnum 0) :: [State]
        addNewer :: [Cell] -> Cell -> [Cell]
        addNewer [] c = [c]
        addNewer (x@(Cell _ m1 n1):xs) c@(Cell _ m2 n2) = (if m1 == m2 && n1 == n2 then [] else [x]) ++ addNewer xs c

modifyGrid :: Grid -> IO () -- opens editor for grid, given size
modifyGrid grid@(Grid _ m n) = do
    drawBorder m n
    putStrLn "WSAD - move, 0 - empty, 1 - wire, 2 - e. head, 3 - e. tail, 9 - finish"
    print2D grid (1, 1)
    -- * main logic
    setCursorPosition 1 1
    hSetBuffering stdin NoBuffering
    showCursor
    newGrid <- fillGrid <$> editionLoop grid (0, 0)
    hSetBuffering stdin LineBuffering
    putStrLn "Enter new Automaton's save path (empty for no save): "
    input <- getLine
    if null input then putStrLn "Didn't save" else saveGrid newGrid input >> putStrLn "Creating new automaton successfull!"

createNewAutomaton :: IO () -- open an editor where you can create your own Grid and save it to file
createNewAutomaton = do
    putStrLn "Enter board size (m n): "
    dims <- map (\x -> read x :: Int) . words <$> getLine
    let [m, n] = dims
    modifyGrid (Grid [] m n)

modifyExistingAutomaton :: IO () -- open and edit a Grid file
modifyExistingAutomaton = do
    putStr "Enter path: "
    path <- getLine
    grid <- loadGrid path
    modifyGrid grid