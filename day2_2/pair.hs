import           Data.List (maximumBy, tails)
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
    then return (cb $ reverse currentLines)
    else do
      newLine <- getLine
      _readAllLines cb (newLine : currentLines)

calculate :: [String] -> String
calculate = maximumBy longerList . map charDiff . crossProduct
  where
    crossProduct xs = [(x, y) | (x:rest) <- tails xs, y <- rest]
    longerList a b = compare (length a) (length b)
    charDiff ("", _) = ""
    charDiff (_, "") = ""
    charDiff ((hx:tx), (hy:ty))
      | hx == hy = hx : charDiff (tx, ty)
      | otherwise = charDiff (tx, ty)
