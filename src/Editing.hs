module Editing
(
    createNewAutomaton
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

modifyGrid :: Grid -> (Int, Int) -> IO Grid -- create new Grid by adding cells using accumulative recursion
modifyGrid grid pos = do
    input <- getChar'
    if input `elem` "wsad" then modifyGrid grid =<< move input (m, n) pos
    else case input of
        x | x `elem` "0123" -> do
            let chosenState = stateList !! digitToInt x
            (putStr . show) chosenState
            cursorBackward 1
            modifyGrid (Grid (cells grid ++ [uncurry (Cell chosenState) pos]) m n) pos
        '9' -> do
            setCursorPosition (m + 3)  0
            if null (cells grid) 
                then return $ Grid [] 0 0
            else return $ Grid (foldl (\c1 c2 -> if row (last c1) == row c2 && column (last c1) == column c2 then init c1 ++ [c2] else c1 ++ [c2]) [head (cells grid)] (tail (cells grid))) m n
        _ -> modifyGrid grid pos
    where
        m = rows grid
        n = columns grid
        stateList = enumFrom (toEnum 0) :: [State]

createNewAutomaton :: IO () -- open an editor where you can create your own Grid and save it to file
createNewAutomaton = do
    putStrLn "Enter board size (m n): "
    dims <- map (\x -> read x :: Int) . words <$> getLine
    let [m, n] = dims
    drawBorder m n
    putStrLn "WSAD - move, 0 - empty, 1 - wire, 2 - e. head, 3 - e. tail, 9 - finish"
    -- * main logic
    setCursorPosition 1 1
    hSetBuffering stdin NoBuffering
    newGrid <- fillGrid <$> modifyGrid (Grid [] m n) (0, 0)
    hSetBuffering stdin LineBuffering
    putStrLn "Enter new Automaton's save path (empty for no save): "
    input <- getLine
    if null input then putStrLn "Didn't save" else saveGrid newGrid input >> putStrLn "Creating new automaton successfull!"