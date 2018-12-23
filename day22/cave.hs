import           System.Environment (getArgs)
import qualified Data.Map.Strict as M
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Query.SP     (sp, spLength)
import Data.Maybe (fromJust)

main :: IO ()
main = do
    [depth, targetX, targetY] <- map read <$> getArgs
    print $ solve1 depth (targetX, targetY)
    print $ solve2 depth (targetX, targetY)

solve1 :: Int -> (Int, Int) -> Int
solve1 depth target@(tx, ty) = sum $ map snd $ filter (\((x, y), _) -> x <= tx && y <= ty) $ M.toList $ getRiskMap depth target

getRiskMap :: Int -> (Int, Int) -> M.Map (Int, Int) Int
getRiskMap depth (targetX, targetY) = M.map getRisk erosionMap
    where
        getErosions :: M.Map (Int, Int) Int -> (Int, Int) -> M.Map (Int, Int) Int
        getErosions m pos = M.insert pos (toErosionLevel $ getGeoIndex m pos) m
        getGeoIndex m (x, y)
            | x == 0 && y == 0 = 0
            | x == targetX && y == targetY = 0
            | y == 0 = x * 16807
            | x == 0 = y * 48271
            | otherwise = (m M.! (x-1, y)) * (m M.! (x,y-1))
        toErosionLevel geoIndex = (geoIndex + depth) `mod` 20183
        erosionMap = foldl getErosions M.empty [(x, y) | x <- [0..(targetX + 30)], y <- [0..(targetY + 30)]]
        getRisk = (`mod` 3) 

-- risk 0: rocky
-- risk 1: wet
-- risk 2: narrow

-- gear 0: neither
-- gear 1: torch
-- gear 2: climbing

solve2 :: Int -> (Int, Int) -> Int
solve2 depth (targetX, targetY) = shortestPathLength
    where
        shortestPathLength = fromJust $ spLength (nodeId (0, 0, 1)) (nodeId (targetX, targetY, 1)) caveGraph
        caveGraph :: Gr () Int
        caveGraph = mkGraph graphNodes graphEdges
        riskMap = getRiskMap depth (targetX, targetY)
        graphNodes = map toNode $ concat $ map expandNodeToType $ M.toList riskMap
        graphEdges = concat $ map expandNodeToTransitions $ M.toList riskMap
        expandNodeToType :: ((Int, Int), Int) -> [(Int, Int, Int)]
        expandNodeToType ((x, y), cellType)
            | cellType == 0 = [(x, y, 1), (x, y, 2)]
            | cellType == 1 = [(x, y, 0), (x, y, 2)]
            | cellType == 2 = [(x, y, 0), (x, y, 1)]
        toNode posGear = (nodeId posGear, ())
        expandNodeToTransitions :: ((Int, Int), Int) -> [(Int, Int, Int)]
        expandNodeToTransitions ((x, y), cellType) = surroundings ++ gearChanges
            where
                gearChanges :: [(Int, Int, Int)]
                gearChanges
                    | cellType == 0 = [(nodeId (x, y, 1), nodeId (x, y, 2), 7), (nodeId (x, y, 2), nodeId (x, y, 1), 7)]
                    | cellType == 1 = [(nodeId (x, y, 0), nodeId (x, y, 2), 7), (nodeId (x, y, 2), nodeId (x, y, 0), 7)]
                    | cellType == 2 = [(nodeId (x, y, 0), nodeId (x, y, 1), 7), (nodeId (x, y, 1), nodeId (x, y, 0), 7)]
                surroundingPositions = filter ((flip M.member) riskMap) $ [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
                surroundings = concat $ map expandToValidTransitions $ zip surroundingPositions $ map (riskMap M.!) $ surroundingPositions
                expandToValidTransitions :: ((Int, Int), Int) -> [(Int, Int, Int)]
                expandToValidTransitions ((x2, y2), otherCellType)
                    | cellType == 0 && otherCellType == 0 = [(nodeId (x, y, 1), nodeId (x2, y2, 1), 1), (nodeId (x, y, 2), nodeId (x2, y2, 2), 1)]
                    | cellType == 1 && otherCellType == 0 = [(nodeId (x, y, 2), nodeId (x2, y2, 2), 1)]
                    | cellType == 2 && otherCellType == 0 = [(nodeId (x, y, 1), nodeId (x2, y2, 1), 1)]
                    | cellType == 0 && otherCellType == 1 = [(nodeId (x, y, 2), nodeId (x2, y2, 2), 1)]
                    | cellType == 1 && otherCellType == 1 = [(nodeId (x, y, 0), nodeId (x2, y2, 0), 1), (nodeId (x, y, 2), nodeId (x2, y2, 2), 1)]
                    | cellType == 2 && otherCellType == 1 = [(nodeId (x, y, 0), nodeId (x2, y2, 0), 1)]
                    | cellType == 0 && otherCellType == 2 = [(nodeId (x, y, 1), nodeId (x2, y2, 1), 1)]
                    | cellType == 1 && otherCellType == 2 = [(nodeId (x, y, 0), nodeId (x2, y2, 0), 1)]
                    | cellType == 2 && otherCellType == 2 = [(nodeId (x, y, 1), nodeId (x2, y2, 1), 1), (nodeId (x, y, 0), nodeId (x2, y2, 0), 1)]




nodeId :: (Int, Int, Int) -> Int
-- there may be collisions for very big graphs
nodeId (x, y, gearType) = (x * 10000 + y) * 10000 + gearType