import           Data.List       (filter, nub)
import           Data.Map.Strict (Map, alter, empty, toList)
import           Data.Maybe      (fromJust, isJust)
import           System.IO       (isEOF)

main :: IO ()
main = do
  res <- readAllLines calculate
  print res

readAllLines :: ([String] -> a) -> IO a
readAllLines cb = _readAllLines cb []

_readAllLines :: ([String] -> a) -> [String] -> IO a
_readAllLines cb currentLines = do
  done <- isEOF
  if done
    then return (cb $ reverse currentLines)
    else do
      newLine <- getLine
      _readAllLines cb (newLine : currentLines)

calculate :: [String] -> Int
calculate =
  mulTuple .
  foldl sumTuples (0, 0) .
  map (foldl count (0, 0)) .
  map nub . map (map snd) . map toList . map (foldl incKey empty)
  where
    incKey curr k = alter newVal k curr
      where
        newVal (Nothing) = Just 1
        newVal (Just x)  = Just (x + 1)
    count :: (Int, Int) -> Int -> (Int, Int)
    count (twos, threes) 2 = (twos + 1, threes)
    count (twos, threes) 3 = (twos, threes + 1)
    count counts _         = counts
    mulTuple (twos, threes) = twos * threes
    sumTuples (s1, s2) (c1, c2) = (s1 + c1, s2 + c2)
