{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

import           Data.List          (groupBy, scanl1, sort, sortBy, maximumBy, group)
import           Data.List.Split    (splitOn)
import           System.Environment (getArgs)

main :: IO ()
main = do
  path <- head <$> getArgs
  strs <- lines <$> readFile path
  let sleepMinutes = expandSleepMinutes $ fillIds $ sort $ map parse strs
  let sleepByGuard = byGuard $ sleepMinutes
  let sleepiestGuard = mostSleepingGuard sleepByGuard
  print $ "Most sleeping guard: " ++ show sleepiestGuard
  print $ "Slept minutes: " ++ show (length (snd sleepiestGuard))
  print $ "Most slept minute: " ++ show (mostSleptMinute sleepiestGuard)
  print $ "Most slept guard in certain minute (minute,times): " ++ show (mostMinuteSleepingGuard sleepByGuard)

data ActionType
  = Begin
  | Sleep
  | Wakeup
  deriving (Eq, Show)

data ActionLog = ActionLog
  { year   :: Int
  , month  :: Int
  , day    :: Int
  , hour   :: Int
  , minute :: Int
  , id     :: Maybe Int
  , action :: ActionType
  } deriving (Eq, Show)

instance Ord ActionLog where
  (ActionLog y1 mon1 d1 h1 m1 _ _) `compare` (ActionLog y2 mon2 d2 h2 m2 _ _) =
    [y1, mon1, d1, h1, m1] `compare` [y2, mon2, d2, h2, m2]

compareMapped :: Ord b => (a -> b) -> a -> a -> Ordering
compareMapped fn x1 x2 = (fn x1) `compare` (fn x2)

mostSleepingGuard = maximumBy (compareMapped $ length . snd)

mostMinuteSleepingGuard = maximumBy (compareMapped third) . map mostSleptMinuteAndTimes
    where
        third (_,_,x) = x

mostSleptMinute = head . maximumBy (compareMapped length) . group . sort . snd

mostSleptMinuteAndTimes (id, mins) = (id, min, times)
    where
    min = mostSleptMinute (id, mins)
    times = length $ filter (== min) mins

byGuard = map asIdMinutesTuple . groupBy groupFn . sortBy sortFn
    where sortFn a b = fst a `compare` fst b
          groupFn a b = fst a == fst b
          asIdMinutesTuple mins = (fst $ head mins, map snd mins)

expandSleepMinutes =
  concat .
  map asMinuteList .
  groupBy sleepWithWakeup . filter (not . (== Begin) . action)
  where
    asMinuteList [(ActionLog {hour = hs, minute = ms, id = (Just id)}), (ActionLog {minute = me})] =
      [ (id, m)
      | m <-
          [(if hs == 23
              then 0
              else ms) .. (me - 1)]
      ]
    sleepWithWakeup a1 a2 = action a1 == Sleep && action a2 == Wakeup

fillIds = scanl1 idFromOld
  where
    idFromOld (ActionLog {id = (Just oldId)}) (ActionLog y2 mon2 d2 h2 m2 Nothing a2) =
      ActionLog y2 mon2 d2 h2 m2 (Just oldId) a2
    idFromOld _ newAction@(ActionLog {id=(Just _)}) = newAction

parse :: String -> ActionLog
parse str = ActionLog {..}
  where
    parts = splitOn " " $ filter (not . (`elem` "[]#")) str
    [year, month, day] = map read $ splitOn "-" $ parts !! 0
    [hour, minute] = map read $ splitOn ":" $ parts !! 1
    id =
      if length parts == 6
        then Just (read $ parts !! 3)
        else Nothing
    actionTell = parts !! 2
    createAction "Guard" = Begin
    createAction "falls" = Sleep
    createAction "wakes" = Wakeup
    action = createAction actionTell