import Test.HUnit

import qualified Data.Map as M

main = do
  runTestTT partOneTests
  text <- readFile "input.txt"
  putStrLn $ "Part one: " ++ (show $ bestdistance text 2503)
  runTestTT partTwoTests
  putStrLn $ "Part two: " ++ (show $ bestscore text 2503)

partOneTests = TestList [
  1120 ~=? (reindeerdist 1000 (14, 10, 127)),
  1056 ~=? (reindeerdist 1000 (16, 11, 162))]

partTwoTests = TestList [
  689 ~=? (bestscore "Comet can fly 14 km/s for 10 seconds, but then must\
                     \ rest for 127 seconds.\nDancer can fly 16 km/s for 11\
                     \ seconds, but then must rest for 162 seconds." 1000)]

bestdistance :: String -> Int -> Int
bestdistance s t = maximum $ map (reindeerdist t) reindeer
  where reindeer = map snd $ getreindeer s

bestscore :: String -> Int -> Int
bestscore s t = maximum $ M.elems scores
  where scores = gettally t 1 emptymap reindeer
        reindeer = getreindeer s
        emptymap = M.fromList $ map (\x -> (fst x, 0)) reindeer

gettally :: Int -> Int -> (M.Map String Int) -> [(String, (Int, Int, Int))] -> (M.Map String Int)
gettally total cum tally reindeer
  | total < cum = tally
  | otherwise = gettally total (cum + 1) (addwinners tally furthest) reindeer
  where furthest = getfurthest cum reindeer

addwinners :: (M.Map String Int) -> [String] -> (M.Map String Int)
addwinners m (x:xs) = addwinners (M.insert x ((m M.! x) + 1) m) xs
addwinners m _ = m

getfurthest :: Int -> [(String, (Int, Int, Int))] -> [String]
getfurthest cum (x:xs) = getfurthest' [fst x] (reindeerdist cum (snd x)) cum xs
getfurthest _ _ = error "empty list"

getfurthest' :: [String] -> Int -> Int -> [(String, (Int, Int, Int))] -> [String]
getfurthest' maxs maxlength time (x:xs) =
  let distance = reindeerdist time (snd x) in
  if maxlength < distance then getfurthest' [fst x] distance time xs
  else if maxlength == distance then getfurthest' ((fst x) : maxs) maxlength time xs
  else getfurthest' maxs maxlength time xs
getfurthest' currwins _ _ _ = currwins

reindeerdist :: Int -> (Int, Int, Int) -> Int
reindeerdist t (ss, st, rt) = ss * st * fullsprints + ss * remainingsprinttime
  where fullsprints = t `div` (st + rt)
        remainingtime = t `mod` (st + rt)
        remainingsprinttime = min remainingtime st

getreindeer :: String -> [(String, (Int, Int, Int))] 
getreindeer s = map parseline $ map words $ lines s
  where 

parseline :: [String] -> (String, (Int, Int, Int))
parseline [name, _, _, speed, _, _, sprinttime, _, _, _, _, _, _, resttime, _] =
  (name, (read speed, read sprinttime, read resttime))
parseline _ = error "failed parse"
