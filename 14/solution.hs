import Test.HUnit

main = do
  runTestTT partOneTests
  text <- readFile "input.txt"
  putStrLn $ "Part one: " ++ (show $ bestdistance text 2503)

partOneTests = TestList [
  1120 ~=? (reindeerdist 1000 (14, 10, 127)),
  1056 ~=? (reindeerdist 1000 (16, 11, 162))]

bestdistance :: String -> Int -> Int
bestdistance s t = maximum $ map (reindeerdist t) reindeer
  where reindeer = getreindeer s

reindeerdist :: Int -> (Int, Int, Int) -> Int
reindeerdist t (ss, st, rt) = ss * st * fullsprints + ss * remainingsprinttime
  where fullsprints = t `div` (st + rt)
        remainingtime = t `mod` (st + rt)
        remainingsprinttime = min remainingtime st

getreindeer :: String -> [(Int, Int, Int)] 
getreindeer s = map parseline $ map words $ lines s
  where 

parseline :: [String] -> (Int, Int, Int)
parseline [_, _, _, speed, _, _, sprinttime, _, _, _, _, _, _, resttime, _] =
  (read speed, read sprinttime, read resttime)
parseline _ = error "failed parse"
