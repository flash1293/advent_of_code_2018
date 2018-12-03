import           Data.List       (maximumBy, tails, (\\))
import           Data.List.Split (splitOn)
import           Data.Map        (alter, empty, toList)
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

data Claim = Claim
  { uid    :: Int
  , x      :: Int
  , y      :: Int
  , width  :: Int
  , height :: Int
  } deriving (Show)

-- calculate :: [String] -> String
calculate strs =
  head $
  allIds \\
  (concat $
   map snd $
   filter ((== (-1)) . fst) $
   map snd $ toList $ foldl setRangeForClaim empty claims)
  where
    claims = map parseClaim strs
    allIds = map uid claims
    setRangeForClaim fabric Claim { uid = currentId
                                  , width = w
                                  , height = h
                                  , x = x
                                  , y = y
                                  } =
      foldl
        (flip (alter (updateField currentId)))
        fabric
        [(x, y) | x <- [x .. (x + w - 1)], y <- [y .. (y + h - 1)]]
    updateField uid Nothing          = Just (uid, [uid])
    updateField uid (Just (_, olds)) = Just (-1, uid : olds)

parseClaim :: String -> Claim
parseClaim = constructClaim . splitOn " "
  where
    constructClaim [(_:uid), _, coord, size] =
      Claim
      { uid = read uid
      , x = getX coord
      , y = getY coord
      , width = getWidth size
      , height = getHeight size
      }
    getX = read . (!! 0) . splitOn ","
    getY = read . (!! 1) . splitOn "," . init
    getWidth = read . (!! 0) . splitOn "x"
    getHeight = read . (!! 1) . splitOn "x"
