import Test.HUnit
import Data.List

main = do
  text <- readFile "input.txt"
  runTestTT tests
  putStrLn . partOne $ text
  --putStrLn . partTwo $ text

tests = TestList [partOneTests, partTwoTests]

partOneTests = TestList [
  TestCase (assertEqual "nice1" True (isNice "ugknbfddgicrmopn")),
  TestCase (assertEqual "nice2" True (isNice "aaa")),
  TestCase (assertEqual "nice3" False (isNice "jchzalrnumimnmhp")),
  TestCase (assertEqual "nice4" False (isNice "haegwjzuvuyypxyu")),
  TestCase (assertEqual "nice5" False (isNice "dvszwmarrgswjxmb"))]

partTwoTests = TestList [
  ]

partOne :: String -> String
partOne t = "Part one: " ++ (show . countNiceLines $ t)

countNiceLines :: String -> Int
countNiceLines t = length . filter isNice $ lines t

isNice :: String -> Bool
isNice s = (containsThreeVowels s) &&
           (containsDoubleLetter s) &&
           (not $ "ab" `isInfixOf` s) &&
           (not $ "cd" `isInfixOf` s) &&
           (not $ "pq" `isInfixOf` s) &&
           (not $ "xy" `isInfixOf` s)

containsThreeVowels :: String -> Bool
containsThreeVowels s = (countVowels s) >= 3

countVowels :: String -> Int
countVowels = foldl (acc) 0
  where acc n c | c `elem` "aeiou" = n + 1
                | otherwise = n

containsDoubleLetter :: String -> Bool
containsDoubleLetter [] = False
containsDoubleLetter (x:xs)
  | null xs = False
  | x == head xs = True
  | otherwise = containsDoubleLetter xs
