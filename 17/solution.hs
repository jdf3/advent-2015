import Test.HUnit
import Data.List

main = do
  runTestTT partOneTests
  text <- readFile "input.txt"
  putStrLn $ "Part one: " ++ (show $ countways (parseconts text) 150)
  runTestTT partTwoTests
  putStrLn $ "Part two: " ++ (show $ countsmallestways (parseconts text) 150)

partOneTests = TestList [
  4 ~=? (countways [20, 15, 10, 5, 5] 25)]

partTwoTests = TestList [
  3 ~=? (countsmallestways [20, 15, 10, 5, 5] 25)]

countways :: [Int] -> Int -> Int
countways l i = length $ validways l i 

validways :: [Int] -> Int -> [[Int]]
validways l i = filter (\x -> (sum x) == i) $ subsequences l

parseconts :: String -> [Int]
parseconts s = map read $ lines s

countsmallestways :: [Int] -> Int -> Int
countsmallestways l i =
  length $ filter (\x -> (length x) == smallest) $ ways 
    where ways = validways l i
          smallest = minimum $ map length ways
