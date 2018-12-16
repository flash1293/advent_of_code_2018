import           Data.Bits          ((.&.), (.|.))
import           Data.Foldable      (toList)
import           Data.List          (findIndices)
import           Data.List.Split    (splitOneOf)
import           Data.Maybe         (fromJust)
import qualified Data.Sequence      as S
import           System.Environment (getArgs)

type VMOp = S.Seq Int -> (Int, Int, Int) -> S.Seq Int

type BeforeAndAfter = (S.Seq Int, [Int], S.Seq Int)

main :: IO ()
main = do
  path <- head <$> getArgs
  strs <- lines <$> readFile path
  let part1 = parseBeforeAndAfters strs
  print $ length $ filter ((>= 3) . length) $ map probe part1

probe :: BeforeAndAfter -> [Int]
probe (before, [_, a, b, c], after) = findIndices outputMatches ops
  where
    outputMatches op = op before (a, b, c) == after

parseBeforeAndAfters :: [String] -> [BeforeAndAfter]
parseBeforeAndAfters strs = map toBeforeAndAfter blocks
  where
    blocks = map toList $ toList $ S.chunksOf 4 $ S.fromList strs
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
