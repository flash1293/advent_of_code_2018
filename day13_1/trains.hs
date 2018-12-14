{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

import           System.Environment (getArgs)
import           Data.List (findIndices, find, sort, intersect, (\\))
import           Data.Maybe (mapMaybe, fromJust)
import           Data.List.Unique (repeated)

main :: IO ()
main = do
    path <- head <$> getArgs
    strs <- lines <$> readFile path
    let (rails, trains) = parse strs
    let simulation = iterate (moveAndRemove rails) (trains, [])
    print $ snd $ fromJust $ find ((> 0) . length . snd) simulation
    -- let firstCollision = head $ snd $ head $ dropWhile ((== 0) . length . snd) simulation
    -- print firstCollision

data Direction = UP | DOWN | LEFT | RIGHT deriving (Show, Eq);

data Train = Train {
    x :: Int,
    y :: Int,
    direction :: Direction,
    nextTurn :: Direction
} deriving (Show);

instance Eq Train where
    (Train x1 y1 _ _) == (Train x2 y2 _ _) = x1 == x2 && y1 == y2

instance Ord Train where
    (Train x1 y1 _ _) `compare` (Train x2  y2 _ _) = [y1, x1] `compare` [y2, x2]

moveAndRemove :: [String] -> ([Train], [Train]) -> ([Train], [Train])
moveAndRemove rails (trains, _) = (withoutCollidedTrains, collisions)
    where
        (newTrains, collisions) = move rails (trains, [])
        withoutCollidedTrains = newTrains \\ collisions

move :: [String] -> ([Train], [Train]) -> ([Train], [Train])
move rails (trains, _) = (newTrains, collisions)
    where
        newTrains = map (moveTrain rails) (sort trains)
        -- this only works for one collision per tick
        collisions = take 1 $ reverse $ repeated $ trains ++ newTrains

moveTrain :: [String] -> Train -> Train
moveTrain rails Train {..}
    | currentPosition == '-' && direction == RIGHT = Train (x + 1) y direction nextTurn
    | currentPosition == '-' && direction == LEFT = Train (x - 1) y direction nextTurn
    | currentPosition == '|' && direction == UP = Train x (y - 1) direction nextTurn
    | currentPosition == '|' && direction == DOWN = Train x (y + 1) direction nextTurn
    | currentPosition == '/' && direction == UP = Train (x + 1) y RIGHT nextTurn
    | currentPosition == '/' && direction == LEFT = Train x (y + 1) DOWN nextTurn
    | currentPosition == '/' && direction == DOWN = Train (x - 1) y LEFT nextTurn
    | currentPosition == '/' && direction == RIGHT = Train x (y - 1) UP nextTurn
    | currentPosition == '\\' && direction == UP = Train (x - 1) y LEFT nextTurn
    | currentPosition == '\\' && direction == LEFT = Train x (y - 1) UP nextTurn
    | currentPosition == '\\' && direction == DOWN = Train (x + 1) y RIGHT nextTurn
    | currentPosition == '\\' && direction == RIGHT = Train x (y + 1) DOWN nextTurn
    | currentPosition == '+' && direction == UP && nextTurn == LEFT = Train (x - 1) y LEFT UP
    | currentPosition == '+' && direction == UP && nextTurn == UP = Train x (y - 1) UP RIGHT
    | currentPosition == '+' && direction == UP && nextTurn == RIGHT = Train (x + 1) y RIGHT LEFT
    | currentPosition == '+' && direction == LEFT && nextTurn == LEFT = Train x (y + 1) DOWN UP
    | currentPosition == '+' && direction == LEFT && nextTurn == UP = Train (x - 1) y LEFT RIGHT
    | currentPosition == '+' && direction == LEFT && nextTurn == RIGHT = Train x (y - 1) UP LEFT
    | currentPosition == '+' && direction == RIGHT && nextTurn == LEFT = Train x (y - 1) UP UP
    | currentPosition == '+' && direction == RIGHT && nextTurn == UP = Train (x + 1) y RIGHT RIGHT
    | currentPosition == '+' && direction == RIGHT && nextTurn == RIGHT = Train x (y + 1) DOWN LEFT
    | currentPosition == '+' && direction == DOWN && nextTurn == LEFT = Train (x + 1) y RIGHT UP
    | currentPosition == '+' && direction == DOWN && nextTurn == UP = Train x (y + 1) DOWN RIGHT
    | currentPosition == '+' && direction == DOWN && nextTurn == RIGHT = Train (x - 1) y LEFT LEFT
    | otherwise = error "rails problem"
    where
        currentPosition = (rails !! y) !! x

parse :: [String] -> ([String], [Train])
parse strs = (cleanedRails, trains)
    where
        removeTrain '>' = '-'
        removeTrain '<' = '-'
        removeTrain '^' = '|'
        removeTrain 'v' = '|'
        removeTrain c = c
        cleanedRails = map (map removeTrain) strs
        trains = concat $ map parseRow $ zip [0..(length strs - 1)] strs

parseRow :: (Int, String) -> [Train]
parseRow (rowNumber, row) = mapMaybe getTrain [0..(length row - 1)]
    where
        getTrain idx = parseTrain idx rowNumber (row !! idx)

parseTrain :: Int -> Int -> Char -> Maybe Train
parseTrain x y c
    | c == '<' = Just $ Train x y LEFT LEFT
    | c == '>' = Just $ Train x y RIGHT LEFT
    | c == '^' = Just $ Train x y UP LEFT
    | c == 'v' = Just $ Train x y DOWN LEFT
    | otherwise = Nothing