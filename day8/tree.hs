{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

import           Data.List.Split    (splitOn)
import           System.Environment (getArgs)

data Node = Node
  { metadata :: [Int]
  , children :: [Node]
  } deriving (Show)

main :: IO ()
main = do
  path <- head <$> getArgs
  str <- readFile path
  let tree = parse str
  print $ metadataSum tree
  print $ value tree

metadataSum :: Node -> Int
metadataSum Node {..} =
  (foldl (+) 0 metadata) + (foldl (flip $ (+) . metadataSum) 0 children)

value :: Node -> Int
value currentNode@Node {children = []} = metadataSum currentNode
value Node {..} = foldl (lookupAndFold) 0 metadata
  where
    lookupAndFold partialSum idx
      | idx <= length children && idx >= 1 =
        (partialSum + value (children !! (idx - 1)))
      | otherwise = partialSum

parse :: String -> Node
parse str = snd $ parse' intList
  where
    intList = map read $ splitOn " " str
    parse' (numberChildNodes:numberMetadataEntries:rest) =
      (restAfterMetaData, Node {..})
      where
        (restAfterChildNodes, children) =
          foldl parseChildNodes (rest, []) [1 .. numberChildNodes]
        (restAfterMetaData, metadata) =
          parseMetadata restAfterChildNodes numberMetadataEntries
        parseChildNodes (currentRest, currentChildNodes) _ =
          (restAfterParse, (currentChildNodes ++ [parsedNode]))
          where
            (restAfterParse, parsedNode) = parse' currentRest
        parseMetadata currentRest numberEntries =
          (drop numberEntries currentRest, take numberEntries currentRest)
    parse' _ = error "Parsing error"
