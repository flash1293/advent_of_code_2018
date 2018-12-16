import           Data.Bits          ((.&.), (.|.))
import           Data.Foldable      (toList)
import           Data.List          (findIndices, find, sort, group)
import           Data.List.Split    (splitOneOf)
import           Data.Maybe         (fromJust, fromMaybe, isJust)
import qualified Data.Sequence      as S
import qualified Data.Map.Strict           as M
import           System.Environment (getArgs)

type VMOp = S.Seq Int -> (Int, Int, Int) -> S.Seq Int

type BeforeAndAfter = (S.Seq Int, [Int], S.Seq Int)

main :: IO ()
main = do
  [path1, path2] <- getArgs
  scenarios <- lines <$> readFile path1
  assembly <- lines <$> readFile path2
  let samples = parseBeforeAndAfters scenarios
  let rules = buildRules samples
  print rules
  let mapping = buildMapping rules
  print mapping
  let program = parseProgram assembly
  let result = foldl (execute mapping) (S.fromList [0, 0, 0, 0]) program
  print result

execute :: [Int] ->  S.Seq Int -> [Int] -> S.Seq Int
execute opMapping mem [op, a, b, c] = (ops !! (opMapping !! op)) mem (a, b, c)

buildMapping :: M.Map Int [Int] -> [Int]
buildMapping rules = work [] [0..15]
  where
    -- depth-first backtracking search of op-code permutation confirmed by samples - doesn't guess (requires at least one sample per op)
    work :: [Int] -> [Int] -> [Int]
    work foundMappings [] = foundMappings
    work foundMappings unmappedSlots = fromMaybe foundMappings $ find ((== 16) . length) possibilities
      where
        opToMap = length foundMappings
        possibilities = map (\slotToFill -> work (foundMappings ++ [slotToFill]) (filter (/= slotToFill) unmappedSlots) ) $ filter inRules unmappedSlots
        inRules slot = isJust $ find (== slot) (fromMaybe [] $ M.lookup opToMap rules)


buildRules :: [BeforeAndAfter] -> M.Map Int [Int]
buildRules samples = M.map (map head . group . sort) $ foldl includeRules M.empty samples
  where
    includeRules rules sample@(_, [op, _, _, _], _) = M.unionWith (++) rules (M.fromList [(op, probe sample)])

probe :: BeforeAndAfter -> [Int]
probe (before, [_, a, b, c], after) = findIndices outputMatches ops
  where
    outputMatches op = op before (a, b, c) == after

parseProgram :: [String] -> [[Int]]
parseProgram = map (map read . splitOneOf " ")

parseBeforeAndAfters :: [String] -> [BeforeAndAfter]
parseBeforeAndAfters scenarios = map toBeforeAndAfter blocks
  where
    blocks = map toList $ toList $ S.chunksOf 4 $ S.fromList scenarios
    toBeforeAndAfter [before, op, after, _] =
      (parseState before, parseOp op, parseState after)
      where
        parseState = S.fromList . map read . init . tail . splitOneOf "[,]"
        parseOp = map read . splitOneOf " "

oprr :: (Int -> Int -> Int) -> VMOp
oprr op mem (a, b, c) =
  S.update c (op (fromJust $ mem S.!? a) (fromJust $ mem S.!? b)) mem

opri :: (Int -> Int -> Int) -> VMOp
opri op mem (a, b, c) = S.update c (op (fromJust $ mem S.!? a) b) mem

opir :: (Int -> Int -> Int) -> VMOp
opir op mem (a, b, c) = S.update c (op a (fromJust $ mem S.!? b)) mem

fromBoolOp :: (Int -> Int -> Bool) -> Int -> Int -> Int
fromBoolOp op a b =
  if op a b
    then 1
    else 0

addr :: VMOp
addr = oprr (+)

addi :: VMOp
addi = opri (+)

mulr :: VMOp
mulr = oprr (*)

muli :: VMOp
muli = opri (*)

banr :: VMOp
banr = oprr (.&.)

bani :: VMOp
bani = opri (.&.)

borr :: VMOp
borr = oprr (.|.)

bori :: VMOp
bori = opri (.|.)

setr :: VMOp
setr = opri (const)

seti :: VMOp
seti = opir (const)

gtir :: VMOp
gtir = opir (fromBoolOp (>))

gtri :: VMOp
gtri = opri (fromBoolOp (>))

gtrr :: VMOp
gtrr = oprr (fromBoolOp (>))

eqri :: VMOp
eqri = opri (fromBoolOp (==))

eqir :: VMOp
eqir = opir (fromBoolOp (==))

eqrr :: VMOp
eqrr = oprr (fromBoolOp (==))

ops :: [VMOp]
ops =
  [ addr
  , addi
  , muli
  , mulr
  , banr
  , bani
  , borr
  , bori
  , setr
  , seti
  , gtir
  , gtri
  , gtrr
  , eqri
  , eqir
  , eqrr
  ]
