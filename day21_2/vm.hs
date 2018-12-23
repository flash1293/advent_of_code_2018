import           Data.Bits          ((.&.), (.|.))
import           Data.Foldable      (toList)
import           Data.List          (findIndices)
import           Data.List.Split    (splitOneOf)
import           Data.Maybe         (fromJust)
import qualified Data.Array      as A
import qualified Data.Map           as M
import qualified Data.Set           as S
import           System.Environment (getArgs)
import           Debug.Trace        (trace)

type VMOp = A.Array Int Int -> (Int, Int, Int) -> A.Array Int Int

main :: IO ()
main = do
  [path, ip] <- getArgs
  strs <- lines <$> readFile path
  let program = parseProgram strs
  -- part A
  print $ evaluate (A.listArray (0,length program) program) (read ip) (A.listArray (0,5) $ take 6 $ repeat 0, S.empty)
  -- part B (theoretically - real solution was found manually by inspecting the algorithm)
  -- print $ evaluate program (read ip) 0 (A.listArray (0,5) $ ((1 :) $ take 5 $ repeat 0))

parseProgram :: [String] -> [(VMOp, [Int])]
parseProgram = map (parseInstruction . splitOneOf " ")
    where
        parseInstruction (opName : params) = (fromJust (M.lookup opName ops), map read params)
        parseInstruction _ = error "invalid instructions"

evaluate :: A.Array Int (VMOp, [Int]) -> Int -> (A.Array Int Int, S.Set (Int)) -> (A.Array Int Int, S.Set (Int))
evaluate program ipReg (mem, haltingStates) = if newIpOOB || isStateLooped then (newMem, S.empty) else (evaluate program ipReg (newMem A.// [(ipReg, newIp)], newHaltingStates))
    where
        ip = mem A.! ipReg
        (currentOp, [a, b, c]) = program A.! ip
        newMem = (currentOp mem (a, b, c))
        newIp = (+1) $ newMem A.! ipReg
        newHaltingStates = if newIp == 28 then (newMem A.! 4) `S.insert` haltingStates else haltingStates
        newIpOOB = if newIp == 28 then trace (show newMem) (newIp < 0 || newIp >= (length program)) else (newIp < 0 || newIp >= (length program))
        isStateLooped = if newIp == 28 then (newMem A.! 4) `S.member` haltingStates else False

oprr :: (Int -> Int -> Int) -> VMOp
oprr op mem (a, b, c) =
  mem A.// [(c, (op (mem A.! a) (mem A.! b)))]

opri :: (Int -> Int -> Int) -> VMOp
opri op mem (a, b, c) = mem A.// [(c, (op (mem A.! a) b))]

opir :: (Int -> Int -> Int) -> VMOp
opir op mem (a, b, c) = mem A.// [(c, (op a (mem A.! b)))]

fromBoolOp :: (Int -> Int -> Bool) -> Int -> Int -> Int
fromBoolOp op a b =
  if op a b
    then 1
    else 0

ops :: M.Map String VMOp
ops = M.fromList [
    ("addr", oprr (+)),
    ("addi", opri (+)),
    ("mulr", oprr (*)),
    ("muli", opri (*)),
    ("banr", oprr (.&.)),
    ("bani", opri (.&.)),
    ("borr", oprr (.|.)),
    ("bori", opri (.|.)),
    ("setr", opri (const)),
    ("seti", opir (const)),
    ("gtir", opir (fromBoolOp (>))),
    ("gtri", opri (fromBoolOp (>))),
    ("gtrr", oprr (fromBoolOp (>))),
    ("eqir", opir (fromBoolOp (==))),
    ("eqri", opri (fromBoolOp (==))),
    ("eqrr", oprr (fromBoolOp (==))) ]