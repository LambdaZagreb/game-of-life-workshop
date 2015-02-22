module Game where

import           Data.List


type Grid = [[Int]]
type Cell = (Int, Int)

createGrid :: Grid -> Grid
createGrid g = undefined

getCellStatus :: Cell -> Grid
getCellStatus c = undefined

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

