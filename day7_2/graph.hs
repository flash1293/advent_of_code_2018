import           Data.Char          (ord)
import           Data.List          (foldl, sort, sortBy)
import           Data.List.Split    (splitOn)
import qualified Data.Map           as M
import           System.Environment (getArgs)

main :: IO ()
main = do
  path <- head <$> getArgs
  strs <- lines <$> readFile path
  let graph = parse strs
  print (solve 5 graph)

solve :: Int -> M.Map Char [Char] -> Int
solve workers = solve' 0 []
  where
    solve' timestep queue graph
      | hasStartingNodeNotInQueue =
        solve' timestep (addNodeToQueue startingNode) graph
      | isWorkInQueueDone queue =
        solve'
          timestep
          (removeDoneWorkFromQueue queue)
          (deleteDoneNode queue (deleteDoneNodeFromVals queue graph))
      | isWorkInQueue queue = solve' (timestep + 1) (bumpQueueWorks queue) graph
      | otherwise = timestep
      where
        deleteDoneNodeFromVals currentQueue currentGraph =
          M.map
            (filter (/= (head $ doneNodesInQueue currentQueue)))
            currentGraph
        deleteDoneNode currentQueue currentGraph =
          M.delete (head $ doneNodesInQueue currentQueue) currentGraph
        hasStartingNodeNotInQueue = (/= 0) $ length $ startingNodes
        startingNode = head startingNodes
        startingNodes =
          filter (\node -> not $ node `elem` (map fst queue)) $
          sort $ map fst $ filter ((== "") . snd) (M.toList graph)
        bumpQueueWorks currentQueue =
          (map (\(node, work) -> (node, work - 1)) $ take workers currentQueue) ++
          (drop workers currentQueue)
        isWorkInQueueDone currentQueue =
          (> 0) $ length $ doneNodesInQueue currentQueue
        isWorkInQueue currentQueue =
          (> 0) $ length $ filter ((> 0) . snd) currentQueue
        doneNodesInQueue currentQueue =
          map fst $ filter ((== 0) . snd) $ currentQueue
        removeDoneWorkFromQueue currentQueue =
          filter
            ((/= (head $ doneNodesInQueue currentQueue)) . fst)
            currentQueue
        addNodeToQueue node = (queue ++ [(node, workNecessaryForNode node)])
        workNecessaryForNode node = 61 + (ord node) - (ord 'A')

parse :: [String] -> M.Map Char [Char]
parse = foldl addToMap M.empty
  where
    addToMap graph str =
      M.unionWith (++) graph $ M.fromList [(target, [source]), (source, "")]
      where
        target = head $ (!! 7) $ splitOn " " str
        source = head $ (!! 1) $ splitOn " " str
