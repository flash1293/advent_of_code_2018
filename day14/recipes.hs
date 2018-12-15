import           Data.Foldable      (toList)
import           Data.List          (drop, find, findIndex, take)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (fromJust)
import qualified Data.Sequence      as S
import           System.Environment (getArgs)

main :: IO ()
main = do
  input <- (read . head <$> getArgs) :: IO Int
  print input
  print $ concat $ map show $ solve1 input
  -- can be this result or minus one of this result
  print $ solve2 input

solve2 :: Int -> Int
-- This is just silly - there is definitely a way better way to write this
solve2 num =
  ((*) (-1)) $
  ((-) (length searchSequence)) $ S.length $ snd $ simulation !! targetIndex
  where
    simulation = iterate tick ((0, 1), S.fromList [3, 7])
    searchSequence = toIntList (show num)
    targetIndex = fromJust $ findIndex isAtEndOfRecipes simulation
    isAtEndOfRecipes (_, recipes) =
      searchSequence ==
      toList (S.drop (S.length recipes - length searchSequence) recipes) ||
      searchSequence ==
      (toList (S.take (length searchSequence) (S.drop (S.length recipes - length searchSequence - 1) recipes)))

solve1 :: Int -> [Int]
solve1 num = take 10 $ drop num $ targetRecipes
  where
    simulation = iterate tick ((0, 1), S.fromList [3, 7])
    targetRecipes =
      toList $ snd $ fromJust $ find ((>= num + 10) . S.length . snd) simulation

tick :: ((Int, Int), S.Seq Int) -> ((Int, Int), S.Seq Int)
tick ((posA, posB), recipes) = ((newPosA, newPosB), newRecipes)
  where
    recipe1 = fromJust $ S.lookup posA recipes
    recipe2 = fromJust $ S.lookup posB recipes
    recipeSum = (show (recipe1 + recipe2))
    newRecipes = recipes S.>< S.fromList (toIntList recipeSum)
    newPosA = (posA + 1 + recipe1) `mod` S.length newRecipes
    newPosB = (posB + 1 + recipe2) `mod` S.length newRecipes

toIntList :: String -> [Int]
toIntList = map read . tail . splitOn ""
