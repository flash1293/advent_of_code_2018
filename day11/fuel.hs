import           Data.List          (maximumBy)
import           System.Environment (getArgs)

main :: IO ()
main = do
  serial <- read . head <$> getArgs
  print $ solve serial

-- TODO: slightly dirty to just search the sizes 10-20.
-- Maybe a better solution would be to cache the grid and just caluclate the sums
solve :: Int -> ((Int, Int, Int), Int)
solve serial = maxGridFuelLevel
  where
    maxGridFuelLevel =
      maximumBy (\a b -> snd a `compare` snd b) $
      map
        gridFuelLevel
        [ (x, y, size)
        | x <- [1 .. 300]
        , y <- [1 .. 300]
        , size <- [10 .. 20]
        , x + size - 1 < 300 && y + size - 1 < 300
        ]
    gridFuelLevel :: (Int, Int, Int) -> ((Int, Int, Int), Int)
    gridFuelLevel pos@(startX, startY, size) =
      ( pos
      , sum $
        map
          fuelLevel
          [ (x, y)
          | x <- [startX .. (startX + size - 1)]
          , y <- [startY .. (startY + size - 1)]
          ])
    fuelLevel :: (Int, Int) -> Int
    fuelLevel (x, y) =
      (((((x + 10) * y) + serial) * (x + 10)) `rem` 1000 `div` 100) - 5
