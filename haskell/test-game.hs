module Main where

import           Game

import           System.Exit (ExitCode (..), exitWith)
import           Test.HUnit  (Assertion, Counts (..), Test (..), runTestTT,
                              (@=?))


exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList createGridTests
       , TestList cellStatusTests
       , TestList invertGridTest
       , TestList getGridSizeTest
       , TestList isValidCoordinateTest
       , TestList getAliveNeighbourCountTests
       , TestList getNextCellStatusTests
       , TestList getNextGenerationTests
       ]


createGridTests :: [Test]
createGridTests =
    [ testCase "Test create grid function" $
      [[Dead,Dead,Dead], [Alive,Alive,Alive]] @=? createGrid [[0,0,0], [1,1,1]]
    ]

cellStatusTests :: [Test]
cellStatusTests =
    [ testCase "Grid [[1, 0], [1, 0]], Getting cell status for (0, 0)" $
      Just Alive @=? getCellStatus (0, 0) testGrid
    , testCase "Grid [[1, 0], [1, 0]], Getting cell status for (0, 1)" $
      Just Dead @=? getCellStatus (0, 1) testGrid
    , testCase "Grid [[1, 0], [1, 0]], Getting cell status for (1, 0)" $
      Just Alive @=? getCellStatus (1, 0) testGrid
    , testCase "Grid [[1, 0], [1, 0]], Getting cell status for (1, 1)" $
      Just Dead @=? getCellStatus (1, 1) testGrid
    ]
    where testGrid = createGrid [[1, 0], [1, 0]]


invertGridTest :: [Test]
invertGridTest =
    [ testCase "Invert Grid [[1, 0], [1, 0]]" $
      [[Dead, Alive], [Dead, Alive]] @=? invertGrid (createGrid [[1, 0], [1, 0]])
    ]

getGridSizeTest :: [Test]
getGridSizeTest =
    [ testCase "Get Grid size 2x2" $
      (2, 2) @=? getGridSize (createGrid [[1, 0], [1, 0]])
    , testCase "Get Grid size 3x3" $
      (3, 3) @=? getGridSize (createGrid [[1, 0, 0], [1, 0, 0], [1, 0, 0]])
    ]

isValidCoordinateTest :: [Test]
isValidCoordinateTest =
    [ testCase "Test valid coordinate" $
      True @=? isValidCoordinate (1, 1) grid
    , testCase "Test invalid coordinate" $
      False @=? isValidCoordinate (1, 2) grid
    ]
    where grid = createGrid [[1, 0], [1, 0]]

getAliveNeighbourCountTests :: [Test]
getAliveNeighbourCountTests =
    [ testCase "Test getting alive neighbour count" $
      1 @=? getAliveNeighbourCount (0, 0) testGrid
    , testCase "Test getting alive neighbour count" $
      3 @=? getAliveNeighbourCount (0, 1) testGrid
    , testCase "Test getting alive neighbour count" $
      0 @=? getAliveNeighbourCount (0, 2) testGrid
    , testCase "Test getting alive neighbour count" $
      1 @=? getAliveNeighbourCount (1, 0) testGrid
    , testCase "Test getting alive neighbour count" $
      3 @=? getAliveNeighbourCount (1, 1) testGrid
    , testCase "Test getting alive neighbour count" $
      1 @=? getAliveNeighbourCount (1, 2) testGrid
    , testCase "Test getting alive neighbour count" $
      1 @=? getAliveNeighbourCount (2, 0) testGrid
    , testCase "Test getting alive neighbour count" $
      1 @=? getAliveNeighbourCount (2, 1) testGrid
    , testCase "Test getting alive neighbour count" $
      0 @=? getAliveNeighbourCount (2, 2) testGrid
    ]
    where testGrid = createGrid [[1, 0, 1], [1, 0, 0], [0, 0, 0]]

getNextCellStatusTests :: [Test]
getNextCellStatusTests =
    [ testCase "Test getting next status" $
      Dead @=? getNextCellStatus (0, 0) testGrid
    , testCase "Test getting next status" $
      Dead @=? getNextCellStatus (0, 1) testGrid
    , testCase "Test getting next status" $
      Dead @=? getNextCellStatus (0, 2) testGrid
    , testCase "Test getting next status" $
      Alive @=? getNextCellStatus (1, 0) testGrid
    , testCase "Test getting next status" $
      Alive @=? getNextCellStatus (1, 1) testGrid
    , testCase "Test getting next status" $
      Alive @=? getNextCellStatus (1, 2) testGrid
    , testCase "Test getting next status" $
      Dead @=? getNextCellStatus (2, 0) testGrid
    , testCase "Test getting next status" $
      Dead @=? getNextCellStatus (2, 1) testGrid
    , testCase "Test getting next status" $
      Dead @=? getNextCellStatus (2, 2) testGrid
    ]
    where testGrid = createGrid [[0, 1, 0], [0, 1, 0], [0, 1, 0]]


getNextGenerationTests :: [Test]
getNextGenerationTests =
        [ testCase "Test getting next generation" $
          grid2 @=? getNextGeneration grid1
        , testCase "Test getting next generation" $
          grid1 @=? getNextGeneration grid2
        ]
        where grid1 = createGrid [[0, 1, 0], [0, 1, 0], [0, 1, 0]]
              grid2 = createGrid [[0, 0, 0], [1, 1, 1], [0, 0, 0]]

