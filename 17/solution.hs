import Test.HUnit
import Data.List

main = do
  runTestTT partOneTests
  text <- readFile "input.txt"
  putStrLn $ "Part one: " ++ (show $ countways (parseconts text) 150)

partOneTests = TestList [
  4 ~=? (countways [20, 15, 10, 5, 5] 25)]

countways :: [Int] -> Int -> Int
countways l i = length $ filter (\x -> (sum x) == i) $ subsequences l

parseconts :: String -> [Int]
parseconts s = map read $ lines s
