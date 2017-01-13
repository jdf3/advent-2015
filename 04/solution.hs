-- MD5 hash with this implementation is glacial. Does the Str constructor
-- required here allocate far too much?
import Test.HUnit
import Data.Hash.MD5

main = do
  let text = "iwrupvqb"
  runTestTT tests
  putStrLn . partOne $ text
  putStrLn . partTwo $ text

tests = TestList [partOneTests, partTwoTests]

partOneTests = TestList [
  TestCase (assertEqual "startsWithFiveZeroes" True (startsWithNZeroes 5 "0000023423423")),
  TestCase (assertEqual "one" 609043 (lowestHashNum 5 "abcdef")),
  TestCase (assertEqual "two" 1048970 (lowestHashNum 5 "pqrstuv"))]

partTwoTests = TestList [
  TestCase (assertEqual "startsWithSixZeroes" True (startsWithNZeroes 6 "0000002345824935")),
  TestCase (assertEqual "startsWithSixZeroesFalse" False (startsWithNZeroes 6 "00000fawleiufh"))]

partOne :: String -> String
partOne t = "Part one: " ++ (show $ lowestHashNum 5 t)

lowestHashNum :: Int -> String -> Int
lowestHashNum = lowestHashNum' 1

lowestHashNum' :: Int -> Int -> String -> Int
lowestHashNum' i n s
  | startsWithNZeroes n $ md5s . Str $ s ++ (show i) = i
  | otherwise = lowestHashNum' (i + 1) n s

startsWithNZeroes :: Int -> String -> Bool
startsWithNZeroes n s = all (\c -> c == '0') $ take n s

partTwo :: String -> String
partTwo t = "Part two: " ++ (show $ lowestHashNum 6 t)
