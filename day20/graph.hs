import           Data.Either                       (fromRight)
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Query.SP     (spTree)
import           Data.List                         (nub)
import qualified Data.Map.Strict                   as M
import qualified Data.Set                          as S
import           System.Environment                (getArgs)
import           Text.ParserCombinators.Parsec

data Direction
  = WEST
  | EAST
  | NORTH
  | SOUTH
  deriving (Show, Eq)

data Discovery
  = Step Direction
  | Way [Discovery]
  | Choice [Discovery]
  deriving (Show, Eq)

type Position = (Int, Int)

type RoomMap = M.Map Position (S.Set Position)

type RoomGraph = Gr () Int

main :: IO ()
main = do
  path <- head <$> getArgs
  str <- readFile path
  let result = fromRight (Way []) (parseDiscovery str)
  let startPosition = (0, 0)
  let roomMap =
        fst $ traverseDiscovery (M.empty, S.singleton startPosition) result
  let roomGraph = toGraph roomMap
  let shortestPaths = spTree (nodeId startPosition) roomGraph
  let shortestPathLengths = map (pred . length . unLPath) shortestPaths
    -- part A
  print $ maximum $ shortestPathLengths
    -- part B
  print $ length $ filter (>= 1000) $ shortestPathLengths

toGraph :: RoomMap -> RoomGraph
toGraph roomMap = mkGraph nodes edges
  where
    nodes = map toNode $ map fst $ M.toList roomMap
    edges = concat $ map fromEntry $ M.toList roomMap
    fromEntry (start, ends) =
      map (\end -> (nodeId start, nodeId end, 1)) $ S.toList ends
    toNode pos = (nodeId pos, ())

nodeId :: Position -> Int
-- there may be collisions for very big graphs
nodeId (x, y) = x * 10000 + y

type TraversalState = (RoomMap, S.Set Position)

traverseDiscovery :: TraversalState -> Discovery -> TraversalState
traverseDiscovery (graph, positions) (Step dir) =
  ( M.unionsWith (S.union) (graph : map newEdges (S.toList positions))
  , S.map move positions)
  where
    move (x, y) =
      case dir of
        WEST  -> (x - 1, y)
        EAST  -> (x + 1, y)
        NORTH -> (x, y - 1)
        SOUTH -> (x, y + 1)
    newEdges pos =
      M.fromList [(pos, S.singleton $ move pos), (move pos, S.singleton pos)]
traverseDiscovery state (Way steps) = foldl traverseDiscovery state steps
traverseDiscovery state@(graph, _) (Choice choices) =
  foldl1 (\(g1, p1) (g2, p2) -> ((M.unionWith (S.union) g1 g2), S.union p1 p2)) $
  map (traverseDiscovery state) choices

parseDiscovery :: String -> Either ParseError Discovery
parseDiscovery str = parse discoveryExpression "(unkown)" str

discoveryExpression :: GenParser Char () Discovery
discoveryExpression = char '^' *> discoveryWay <* char '$'

discoveryWay :: GenParser Char () Discovery
discoveryWay = Way <$> many (discoveryChoice <|> discoveryStep)

discoveryStep :: GenParser Char () Discovery
discoveryStep =
  Step <$>
  choice
    [ char 'W' >> return WEST
    , char 'E' >> return EAST
    , char 'N' >> return NORTH
    , char 'S' >> return SOUTH
    ]

discoveryChoice :: GenParser Char () Discovery
discoveryChoice =
  Choice <$> (char '(' *> (discoveryWay `sepBy` char '|') <* char ')')
