import Test.HUnit
import qualified Data.Set as Set

main = do
  text <- readFile "input.txt"
  runTestTT tests
  putStrLn . partOne $ text
  putStrLn . partTwo $ text

tests = TestList [partOneTests, splitTests, partTwoTests]

partOneTests = TestList [
  TestCase (assertEqual "simple" 2 (countHouses ">")),
  TestCase (assertEqual "medium" 4 (countHouses "^>v<")),
  TestCase (assertEqual "longer" 2 (countHouses "^v^v^v^v^v"))]

splitTests = TestList [
  TestCase (assertEqual "split 1" [0, 1, 3] (evenIndexed [0, -1, 1, 2, 3])),
  TestCase (assertEqual "split 2" [-1, 2] (oddIndexed [0, -1, 1, 2, 3]))]

partTwoTests = TestList [
  TestCase (assertEqual "simple" 3 (countHousesRobo "^v")),
  TestCase (assertEqual "medium" 3 (countHousesRobo "^>v<")),
  TestCase (assertEqual "longer" 11 (countHousesRobo "^v^v^v^v^v"))]

partOne :: String -> String
partOne t = "Part one: " ++ (show . countHouses $ t)

countHouses :: String -> Int
countHouses s = length . unique . getHouses $ s

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

getHouses :: String -> [(Int, Int)]
getHouses s = foldl (acc) [(0, 0)] s
  where acc l c = getNextPosition (head l) c : l

getNextPosition :: (Int, Int) -> Char -> (Int, Int)
getNextPosition (x, y) c
    | c == '>' = (x + 1, y)
    | c == 'v' = (x, y - 1)
    | c == '<' = (x - 1, y)
    | c == '^' = (x, y + 1)
    | otherwise = (x, y)

partTwo :: String -> String
partTwo s = "Part two: " ++ (show . countHousesRobo $ s)   

takeNth n xs = case drop (n - 1) xs of
               (y:ys) -> y : takeNth n ys
               [] -> []

evenIndexed (x:xs) = x : takeNth 2 xs
evenIndexed [] = []

oddIndexed xs = takeNth 2 xs

countHousesRobo :: String -> Int
countHousesRobo s = length . unique . getHousesRobo $ s

getHousesRobo :: String -> [(Int, Int)]
getHousesRobo s = (getHouses . evenIndexed $ s) ++ (getHouses . oddIndexed $ s)
