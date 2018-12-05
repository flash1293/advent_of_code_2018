import           Data.Char          (isUpper, toLower)
import           System.Environment (getArgs)
import           Data.List          (group, sort)

main :: IO ()
main = do
  path <- head <$> getArgs
  polymer <- head . lines <$> readFile path
  let reactedPolymer = react polymer
  print $ "Reacted polymer: " ++ reactedPolymer
  print $ "Length: " ++ show (length reactedPolymer)
  let reactedPolymersWithoutUnits = map react $ map (polymerWithoutUnit polymer) $ getUnits polymer
  let shortestPolymerLength = minimum $ map length reactedPolymersWithoutUnits
  print $ "Shortest chain: " ++ show shortestPolymerLength

react :: String -> String
react = foldr reactPair []
  where
    reactPair c [] = [c]
    reactPair c (h:t) =
      if (isUpper h /= isUpper c) &&
         toLower h == toLower c
        then t
        else (c : h : t)

polymerWithoutUnit :: String -> Char -> String
polymerWithoutUnit polymer unit = filter ((/= unit) . toLower) polymer

getUnits :: String -> [Char]
getUnits = map head . group . sort . map toLower