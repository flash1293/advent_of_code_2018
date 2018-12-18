import           Data.List          (findIndex, group, sort)
import qualified Data.Map.Strict    as M
import           Data.Maybe         (fromJust, mapMaybe)
import qualified Data.Set           as S
import           System.Environment (getArgs)

data CellType
  = Open
  | Trees
  | Lumber
  deriving (Eq, Ord)

type Position = (Int, Int)

type AreaMap = M.Map Position CellType

main :: IO ()
main = do
  path <- head <$> getArgs
  strs <- lines <$> readFile path
  let area = parse strs
  let simulation = iterate apply area
    -- part A
  print $ resourceValue $ simulation !! 10
    -- part B
  let (loopStart, loopEnd) = findLoop simulation
  let loopLength = loopEnd - loopStart
  let offset = (1000000000 - loopEnd) `rem` loopLength
  print $ resourceValue $ simulation !! (loopEnd + offset)

resourceValue :: AreaMap -> Int
resourceValue = foldl1 (*) . tail . countCells

countCells :: AreaMap -> [Int]
countCells = map length . group . sort . map snd . M.toList

findLoop :: [AreaMap] -> (Int, Int)
findLoop simulation = (firstIndex, secondIndex)
  where
    search (current:rest) oldVals =
      if S.member current oldVals
        then current
        else search rest (S.insert current oldVals)
    search [] _ =
      error "implementation error in findLoop - end of infinite list reached"
    loopedState = search simulation S.empty
    firstIndex = fromJust $ findIndex (== loopedState) simulation
    secondIndex =
      1 + firstIndex +
      (fromJust $ findIndex (== loopedState) (drop (firstIndex + 1) simulation))

apply :: AreaMap -> AreaMap
apply area = M.mapWithKey transform area
  where
    transform pos cell = byRule cell $ map snd $ M.toList $ perimeter pos
    perimeter (x, y) =
      M.unionWith (+) emptyPerimeter $
      M.fromListWith (+) $
      map (\cell -> (cell, 1)) $
      mapMaybe ((flip M.lookup) area) $
      [ (px, py)
      | px <- [(x - 1) .. (x + 1)]
      , py <- [(y - 1) .. (y + 1)]
      , px /= x || py /= y
      ]
    emptyPerimeter :: M.Map CellType Int
    emptyPerimeter = M.fromList [(Open, 0), (Trees, 0), (Lumber, 0)]
    byRule Open [_, numTrees, _] =
      if numTrees >= 3
        then Trees
        else Open
    byRule Trees [_, _, numLumber] =
      if numLumber >= 3
        then Lumber
        else Trees
    byRule Lumber [_, numTrees, numLumber] =
      if numTrees >= 1 && numLumber >= 1
        then Lumber
        else Open
    byRule _ _ = error "implementation error in apply"

parse :: [String] -> AreaMap
parse input = foldl addRow M.empty $ zip input [0 .. (length input - 1)]
  where
    addRow area (row, y) = M.union area (M.fromList $ parseRow y row)
    parseRow y row =
      map (\(x, cell) -> ((x, y), cell)) $
      zip [0 .. (length row - 1)] $ map parseChar row
    parseChar '.' = Open
    parseChar '|' = Trees
    parseChar '#' = Lumber
    parseChar _   = error "Parsing error - Unexpected area character"
