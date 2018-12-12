import           Data.List          (findIndex)
import           Data.List.Split    (splitOn)
import qualified Data.Map.Strict    as M
import           Data.Maybe         (fromJust, fromMaybe)
import           System.Environment (getArgs)

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p =
  foldr
    (\x xs ->
       if null xs && p x
         then []
         else x : xs)
    []

main :: IO ()
main = do
  path <- head <$> getArgs
  strs <- lines <$> readFile path
  let initialState = strs !! 0
  let rules = parseRules $ drop 2 strs
  let simulation = iterate (move rules) (initialState, 0)
  -- part A
  -- let timesteps = take 21 simulation
  -- mapM_ print $ timesteps
  -- print $ score $ last timesteps
  -- part B
  let score10k = score $ simulation !! 10000
  let score20k = score $ simulation !! 20000
  let score50b =
        score10k + (score20k - score10k) * ((50000000000 `div` 10000) - 1)
  print score50b

score :: (String, Int) -> Int
score (state, indexOffset) =
  foldl (+) 0 $
  map
    (\(plant, index) ->
       if plant == '#'
         then (index - 1 + indexOffset)
         else 0) $
  zip state [1 .. (length state)]

move :: M.Map String Char -> (String, Int) -> (String, Int)
move rules (state, indexOffset) =
  (cleanNextState, indexOffset + firstPlantIndex - 3)
  where
    augmentedState = "....." ++ state ++ "....."
    applyRule idx = lookupRule $ take 5 $ drop idx augmentedState
    lookupRule statePart = fromMaybe '.' $ M.lookup statePart rules
    nextState = map applyRule [0 .. (length augmentedState - 4)]
    firstPlantIndex = fromJust $ findIndex (== '#') nextState
    cleanNextState = drop firstPlantIndex $ dropWhileEnd (== '.') nextState

parseRules :: [String] -> M.Map String Char
parseRules = M.fromList . map toTuple . map (splitOn " => ")
  where
    toTuple [a, b] = (a, head b)
