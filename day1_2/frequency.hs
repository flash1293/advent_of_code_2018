import           Data.List  (find, scanl)
import           Data.Maybe (fromJust, isJust)
import           Data.Set   (empty, insert, member)
import           System.IO  (isEOF)

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
  fst .
  fromJust .
  find (\(h, s) -> member h s) .
  scanl (\(h, s) e -> (h + e, insert h s)) (0, empty) .
  cycle . map readPositiveNegative

readPositiveNegative :: String -> Int
readPositiveNegative ('+':n) = read n
readPositiveNegative ('-':n) = -1 * read n
readPositiveNegative _       = error "number with invalid format"

isHeadInTail :: Eq a => [a] -> Bool
isHeadInTail (h:t) = isJust $ find (\x -> h == x) t
isHeadInTail []    = False
