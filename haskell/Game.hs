module Game where

import           Data.List

data Status = Dead | Alive deriving (Show, Eq)

type GridSize = (Int, Int)
type Grid = [[Status]]
type Cell = (Int, Int)

getGridSize :: Grid -> GridSize
getGridSize g = undefined

int2Status :: Int -> Status
int2Status e = undefined

createGrid :: [[Int]] -> Grid
createGrid g = undefined

getCellStatus :: Cell -> Grid -> Maybe Status
getCellStatus c g = undefined

flipCellStatus :: Status -> Status
flipCellStatus s = undefined

invertGrid :: Grid -> Grid
invertGrid g = undefined

isValidCoordinate :: Cell -> Grid -> Bool
isValidCoordinate c g = undefined

getAliveNeighbourCount :: Cell -> Grid -> Int
getAliveNeighbourCount c g = undefined

getNextCellStatus :: Cell -> Grid -> Status
getNextCellStatus c g = undefined

getNextGeneration :: Grid -> Grid
getNextGeneration g = undefined

