{-# LANGUAGE NegativeLiterals, RecordWildCards, NamedFieldPuns, FlexibleContexts #-}
module Life where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Control.Concurrent (threadDelay)
import Data.Foldable (foldl')

type Pos = (Int, Int)
data Grid = Grid { livePositions :: Set.Set Pos
                 , nRows :: Int
                 , nCols :: Int
                 }


updateGrid :: Grid -> Grid
updateGrid g = g { livePositions = (newLivePositions g) }


newLivePositions :: Grid -> Set.Set Pos
newLivePositions g = Map.keysSet $ (Map.filterWithKey survives) $ getNearbyPopulation g
  where survives p nearbyPop =
          (isAlive p g && nearbyPop == 2) ||
          nearbyPop == 3


isAlive :: Pos -> Grid -> Bool
isAlive p Grid { livePositions} = Set.member p livePositions


getNearbyPopulation :: Grid -> Map.Map Pos Int
getNearbyPopulation g@Grid { livePositions } =
  foldl' updateAll Map.empty livePositions
  where
    updateOne m p = Map.insertWith (+) p 1 m
    updateAll m p = foldl' updateOne m (getNeighbors p g)


getNeighbors :: Pos -> Grid -> [Pos]
getNeighbors (row, col) g =
  [wrapPos (row + r, col + c) g |
    r <- [-1..1],
    c <- [-1..1],
    not (r == 0 && c == 0)]


wrapPos :: Pos -> Grid -> Pos
wrapPos (row, col) Grid { nRows, nCols } =
  (row `mod` nRows, col `mod` nCols)



gridFromCells :: [Pos] -> Grid
gridFromCells cells = Grid
  { livePositions = (Set.fromList cells)
  , nRows = 20
  , nCols = 20
  }


showGrid :: Grid -> String
showGrid Grid {..} =
  let
    showPos p = if (Set.member p livePositions) then " O " else "   "
    showRow r = List.concatMap showPos [(r, c) | c <- [1..nCols]]
  in List.intercalate "\n" (map showRow [1..nRows])


clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

sleepSeconds :: Double -> IO ()
sleepSeconds seconds = threadDelay $ floor $ seconds * 1000000


initialCells :: [Pos]
initialCells = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]


liveGame :: IO ()
liveGame =
  let go grid = do
        let display = showGrid grid
        clearScreen
        putStrLn display
        sleepSeconds 0.25
        go $ updateGrid grid
  in go $ gridFromCells initialCells
