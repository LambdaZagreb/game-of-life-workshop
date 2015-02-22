module Game where

import           Data.List

type GridSize = (Int, Int)
type Grid = [[Int]]
type Cell = (Int, Int)

getGridSize :: Grid -> GridSize
getGridSize g = undefined

createGrid :: Grid -> Grid
createGrid g = undefined

getCellStatus :: Cell -> Grid -> Int
getCellStatus c g = undefined

invertGrid :: Grid -> Grid
invertGrid g = undefined

isValidCoordinate :: Cell -> Grid -> Bool
isValidCoordinate c g = undefined

getAliveNeighbourCount :: Cell -> Grid -> Int
getAliveNeighbourCount c g = undefined

getNextCellStatus :: Cell -> Grid -> Int
getNextCellStatus c g = undefined

getNextGeneration :: Grid -> Grid
getNextGeneration g = undefined

