import           Data.List          (foldl, sort)
import           Data.List.Split    (splitOn)
import qualified Data.Map           as M
import           System.Environment (getArgs)

main :: IO ()
main = do
  path <- head <$> getArgs
  strs <- lines <$> readFile path
  let graph = parse strs
  print (solve graph)

solve :: M.Map Char [Char] -> [Char]
solve = reverse . solve' ""
  where
    solve' prefix graph
      | hasStartingNode =
        solve'
          ((head startingNodes) : prefix)
          (deleteStartingNode (deleteStartingNodeFromVals graph))
      | otherwise = prefix
      where
        deleteStartingNodeFromVals currentGraph =
          M.map (filter (/= startingNode)) currentGraph
        deleteStartingNode currentGraph = M.delete startingNode currentGraph
        hasStartingNode = (/= 0) $ length $ startingNodes
        startingNode = head startingNodes
        startingNodes = sort $ map fst $ filter ((== "") . snd) (M.toList graph)

parse :: [String] -> M.Map Char [Char]
parse = foldl addToMap M.empty
  where
    addToMap graph str =
      M.unionWith (++) graph $ M.fromList [(target, [source]), (source, "")]
      where
        target = head $ (!! 7) $ splitOn " " str
        source = head $ (!! 1) $ splitOn " " str
