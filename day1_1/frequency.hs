import           System.IO (isEOF)

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
    then return (cb currentLines)
    else do
      newLine <- getLine
      _readAllLines cb (newLine : currentLines)

calculate :: [String] -> Int
calculate = foldl (+) 0 . map readPositiveNegative

readPositiveNegative :: String -> Int
readPositiveNegative ('+':n) = read n
readPositiveNegative ('-':n) = -1 * read n
readPositiveNegative _       = error "number with invalid format"
