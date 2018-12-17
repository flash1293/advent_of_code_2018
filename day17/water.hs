import qualified Data.Map.Strict           as M
import qualified Data.Set           as S
import           System.Environment (getArgs)
import           Data.List (find)
import           Data.List.Split    (splitOneOf, splitOn)
import           Data.Maybe (fromMaybe, fromJust, isNothing)

-- ((x1, x2), (y1, y2))
type ClayPosition = ((Int, Int), (Int, Int))
type Position = (Int, Int)

main :: IO ()
main = do
    path <- head <$> getArgs
    strs <- lines <$> readFile path
    let clay = parse strs
    let groundMap = buildClayMap clay
    let flowMap = S.fromList [(500,1)]
    let maxY = maximum $ map snd $ S.toList groundMap
    let minY = minimum $ map snd $ S.toList groundMap
    let simulation = iterate (simulateAll maxY) (groundMap, flowMap)
    let simulationPairs = zip simulation (tail simulation)
    let (groundAtRest, flowAtRest) = fst $ fromJust $ find (\((g1, f1), (g2, f2)) -> S.size g1 == S.size g2 && S.size f1 == S.size f2) simulationPairs
    -- dirty min-bound check (assumes the water from the spring doesnt hit the first clay block)
    let water = S.size flowAtRest + S.size (S.difference groundAtRest groundMap) - (minY - 1)
    print water
    let restingWater = S.size (S.difference groundAtRest groundMap)
    print restingWater

buildClayMap :: [ClayPosition] -> S.Set Position
buildClayMap = S.unions . map expand
    where
        expand ((x1, x2), (y1, y2)) = S.fromList [(x, y) | x <- [x1..x2], y <- [y1..y2]]

simulateAll :: Int -> (S.Set Position, S.Set Position) -> (S.Set Position, S.Set Position)
simulateAll maxY initialState@(_, initialFlow) = (newGround, S.difference newFlow newGround)
    where
        (newGround, newFlow) = foldl (simulate maxY) initialState initialFlow

-- |The 'simulate' function takes the map of clay and resting water, the map of dynamic materials (water),
-- |a current position and returns a modified map of dynamic materials for the next step
simulate :: Int -> (S.Set Position, S.Set Position) -> Position -> (S.Set Position, S.Set Position)
simulate maxY (ground, flowingWater) (x, y) = (updatedGround, updatedFlowingWater)
    where
        isGroundBelow = S.member (x, y + 1) ground
        isGroundLeft = S.member (x - 1, y) ground
        isGroundRight = S.member (x + 1, y) ground
        isFlowingWaterBelow = S.member (x, y + 1) flowingWater
        isWaterLeft = S.member (x - 1, y) flowingWater
        isWaterRight = S.member (x + 1, y) flowingWater
        flowsToLeft = isGroundBelow && not isGroundLeft && not isWaterLeft
        flowsToRight = isGroundBelow && not isGroundRight && not isWaterRight
        flowsDown = y < maxY && (not isGroundBelow) && (not isFlowingWaterBelow)
        becomesRestingWater = comesToRest ground flowingWater (x, y)
        updatedGround = updateSet ground (x, y) False False False becomesRestingWater
        updatedFlowingWater = updateSet flowingWater (x, y) flowsToLeft flowsToRight flowsDown False
        
updateSet :: S.Set Position -> Position -> Bool -> Bool -> Bool -> Bool -> S.Set Position
updateSet ground (x, y) left right down spot = S.unions [ground, leftSet, rightSet, downSet, spotSet]
    where
        leftSet = if left then S.fromList [(x-1,y)] else S.empty
        rightSet = if right then S.fromList [(x+1,y)] else S.empty
        downSet = if down then S.fromList [(x,y+1)] else S.empty
        spotSet = if spot then S.fromList [(x,y)] else S.empty

comesToRest :: S.Set Position -> S.Set Position -> Position -> Bool
comesToRest ground flowingWater (x, y) = hasLeftFringe && hasRightFringe && hasSupport
    where
        leftBoundary = head $ dropWhile (\c -> S.member (c, y) flowingWater) (iterate pred x)
        rightBoundary = head $ dropWhile (\c -> S.member (c, y) flowingWater) (iterate succ x)
        hasLeftFringe = S.member (leftBoundary, y) ground
        hasRightFringe = S.member (rightBoundary, y) ground
        hasSupport = isNothing $ find (\c -> not (S.member (c, y + 1) ground)) [(leftBoundary + 1)..(rightBoundary - 1)]

parse :: [String] -> [ClayPosition]
parse = map (mkTuples . splitOneOf "=, ")
    where
        mkTuples ["y", y, _, _, x] = (mkTuple x, mkTuple y)
        mkTuples ["x", x, _, _, y] = (mkTuple x, mkTuple y)
        mkTuple str = mkRange $ map read $ splitOn ".." str
        mkRange [v] = (v, v)
        mkRange [v1, v2] = (v1, v2)