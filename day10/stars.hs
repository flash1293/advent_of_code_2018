{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

import           Data.List          (findIndex, scanl)
import           Data.List.Split    (splitOn, splitOneOf)
import           Data.Maybe         (fromJust, isJust)
import           Data.String.Utils  (strip)
import           System.Environment (getArgs)

main :: IO ()
main = do
  path <- head <$> getArgs
  strs <- lines <$> readFile path
  let startingPoints = parse strs
  let (endPoints, timestep) = solve startingPoints
  printPoints endPoints
  print $ show timestep

type Bounds = (Int, Int, Int, Int)

data Point = Point
  { x  :: Int
  , y  :: Int
  , vx :: Int
  , vy :: Int
  } deriving (Show)

printPoints :: [Point] -> IO ()
printPoints points = mapM_ print rows
  where
    (minX, minY, maxX, maxY) = bounds points
    rows = map buildRow [minY .. maxY]
    buildRow y = map (getChar y) [minX .. maxX]
    getChar y x =
      if isJust (findIndex (lookup x y) points)
        then '#'
        else '.'
    lookup targetX targetY Point {x, y} = targetX == x && targetY == y

solve :: [Point] -> ([Point], Int)
solve points = (tippingPoint, tippingPointIndex)
  where
    startingBounds = bounds points
    pointsThroughTime =
      scanl update (points, startingBounds, startingBounds) $ iterate (+ 1) 1
    isOldBoundSmaller (_, newBounds, oldBounds) =
      boundArea oldBounds < boundArea newBounds
    tippingPointIndex =
      (fromJust $ findIndex isOldBoundSmaller pointsThroughTime) - 1
    (tippingPoint, _, _) = pointsThroughTime !! tippingPointIndex

update :: ([Point], Bounds, Bounds) -> Int -> ([Point], Bounds, Bounds)
update (points, oldBounds, _) _ = (newPoints, bounds newPoints, oldBounds)
  where
    newPoints = map movePoint points

boundArea :: Bounds -> Int
boundArea (x1, y1, x2, y2) = (x2 - x1) * (y2 - y1)

movePoint :: Point -> Point
movePoint Point {..} = Point (x + vx) (y + vy) vx vy

bounds :: [Point] -> Bounds
bounds points = foldl updateBounds (sampleX, sampleY, sampleX, sampleY) points
  where
    Point {x = sampleX, y = sampleY} = head points
    updateBounds (minX, minY, maxX, maxY) Point {x, y} =
      (min x minX, min y minY, max x maxX, max y maxY)

parse :: [String] -> [Point]
parse strs = map toPoint strs
  where
    toPoint str = Point {..}
      where
        parts = splitOneOf "<>" str
        getXY part = map read $ map strip $ splitOn ", " $ parts !! part
        [x, y] = getXY 1
        [vx, vy] = getXY 3
