import qualified Data.List          as L
import qualified Data.Map.Strict    as M
import qualified Data.Sequence      as S
import           System.Environment (getArgs)

main :: IO ()
main = do
  [marbles, players] <- map read <$> getArgs
  print $ play marbles players

play :: Int -> Int -> Int
play marbles players = L.maximum $ map snd $ M.toList finishedScores
  where
    (finishedScores, _, _) = L.foldl move startingState [1 .. marbles]
    startingState = (M.empty, S.fromList [0], 0)
    move (scores, ring, currentMarble) newMarble
      | newMarble `mod` 23 == 0 =
        ( M.unionWith
            (+)
            scores
            (M.fromList [(currentPlayer, removedMarble + newMarble)])
        , S.deleteAt removalIndex ring
        , removalIndex)
      | otherwise =
        (scores, S.insertAt insertionIndex newMarble ring, insertionIndex)
      where
        currentPlayer = ((newMarble - 1) `mod` players) + 1
        newMarbleIndex = (currentMarble + 2) `mod` length ring
        insertionIndex =
          if newMarbleIndex == 0
            then length ring
            else newMarbleIndex
        removalIndex = (currentMarble + length ring - 7) `mod` length ring
        removedMarble = S.index ring removalIndex
