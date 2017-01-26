import Test.HUnit
import Data.List

main = do 
  runTestTT partOneTests
  putStrLn $ "Part one: " ++ (show partOne)
  putStrLn $ "Part two: " ++ (show partTwo)

partOneTests = TestList [
  "11" ~=? (say "1"),
  "21" ~=? (say "11"),
  "1211" ~=? (say "21"),
  "111221" ~=? (say "1211"),
  "312211" ~=? (say "111221")
  ]

input :: String
input = "1113122113"

partOne :: Int
partOne = length $ sayn input 40

partTwo :: Int
partTwo = length $ sayn input 50 

sayn :: String -> Int -> String
sayn s i = (iterate say s) !! i

say :: String -> String
say s =
    concatMap sayGroup $ group s
    where
      sayGroup ds = show (length ds) ++ [head ds]
