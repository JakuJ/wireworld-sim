module Automaton
(
    State, Cell (..), Grid (..), Automaton (..), stateList,
    saveGrid, loadGrid,
    fillGrid,
    nextWireworld,
    nextGeneration,
    calculatePeriod
) where

import Data.Tuple           (swap)
import Data.List            (find, delete)
import Data.List.Split      (chunksOf)
import Data.Maybe           (fromMaybe, fromJust, isJust, isNothing)

data State = Empty | Wire | Head | Tail deriving (Eq, Enum)

stateList = enumFrom (toEnum 0) :: [State]

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
    cellTypes :: [State],
    evolutionFunction :: Automaton -> Automaton,
    grid :: Grid,
    generation :: Int
}

nextGeneration :: Automaton -> Automaton
nextGeneration a = evolutionFunction a a

instance Eq Automaton where
    a == b = grid a == grid b && cellTypes a == cellTypes b

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

fillGrid :: Grid -> Grid -- fills Grid which only has non-Empty cells to m x n size with Empty cells
fillGrid (Grid cells ms ns) = Grid (map (\(x, y) -> (fromMaybe (Cell Empty x y) (found x y))) (cart [0 .. ms - 1] [0 .. ns - 1])) ms ns
    where
        found x y = find (\(Cell s m n) -> m == x && n == y) cells
        cart :: [a] -> [b] -> [(a, b)] -- Cartesian product of two vectors
        cart xs ys = [(x, y) | x <- xs, y <- ys]

getNeighbours :: Grid -> Cell -> [Cell] -- get [cell] ++ its Moore neighbourhood
getNeighbours grid cell = [cells grid!!(columns grid * m + n) | m <- ms, n <- ns]
    where
        x = row cell
        y = column cell
        ms = filter (\z -> z >= 0 && z < rows grid) [x - 1, x, x + 1]
        ns = filter (\z -> z >= 0 && z < columns grid) [y - 1, y, y + 1]

nextWireworld :: Automaton -> Automaton -- generate next generation of a Wireworld automaton
nextWireworld (Automaton s evo g@(Grid cs m n) gen) = Automaton s evo (Grid (map ruleset cs) m n) (gen + 1)
    where
        ruleset :: Cell -> Cell -- determines how cells change in time
        ruleset c@(Cell s x y) 
            | s == Empty = c
            | s == Head = Cell Tail x y
            | s == Tail = Cell Wire x y
            | s == Wire = if heads c == 1 || heads c == 2 then Cell Head x y else Cell Wire x y
            where
                heads c = length $ filter (\(Cell state _ _) -> state == Head) (getNeighbours g c) -- count electron heads in cell's neighbourhood

calculatePeriod :: Automaton -> Maybe (Int, Int) -- if the automaton loops before 500 iterations, return period of this loop
calculatePeriod automat = recursiveCheck automat []
    where
        recursiveCheck :: Automaton -> [Automaton] -> Maybe (Int, Int) -- takes current grid and a list of all previous grids
        recursiveCheck a@(Automaton _ _ g _) as
            | length as >= 500 = Nothing
            | a `elem` as = Just $ tuple (length (takeWhile (/=a) as)) $ (+) 1 $ length $ takeWhile (/= a) $ tail $ dropWhile (/= a) as
            | otherwise = recursiveCheck (nextGeneration a) $ as ++ [a]
            where
                tuple :: a -> b -> (a, b)
                tuple a b = (a, b)