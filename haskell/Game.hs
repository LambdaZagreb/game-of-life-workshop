module Game where

import           Data.List

data Status = Dead | Alive deriving (Show, Eq)

type GridSize = (Int, Int)
type Grid = [[Status]]
type Cell = (Int, Int)

getGridSize :: Grid -> GridSize
getGridSize g = (length (g !! 0), length g)

int2Status :: Int -> Status
int2Status e = case e of
                   1 -> Alive
                   0 -> Dead
                   otherwise -> error "Invalid input for Grid. Valid choices are 0 and 1."

createGrid :: [[Int]] -> Grid
createGrid g = map (map int2Status) g

getCellStatus :: Cell -> Grid -> Maybe Status
getCellStatus c g = case (isValidCoordinate c g) of
                        False -> Nothing
                        True -> Just ((g !! (fst c)) !! (snd c))

flipCellStatus :: Status -> Status
flipCellStatus s = case s of
                       Dead -> Alive
                       Alive -> Dead

invertGrid :: Grid -> Grid
invertGrid g = map (map flipCellStatus) g

isValidCoordinate :: Cell -> Grid -> Bool
isValidCoordinate c g = case c of
                            (x, y) | (x < x_size) && (y < y_size) && (x >= 0) && (y >= 0) -> True
                            otherwise -> False
                        where x_size = fst (getGridSize g)
                              y_size = snd (getGridSize g)

getAliveNeighbourCount :: Cell -> Grid -> Int
getAliveNeighbourCount c g = undefined

getNextCellStatus :: Cell -> Grid -> Status
getNextCellStatus c g = undefined

getNextGeneration :: Grid -> Grid
getNextGeneration g = undefined

