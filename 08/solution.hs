import Test.HUnit

main = do
  runTestTT partOneTests
  text <- readFile "input.txt"
  putStrLn $ "Part one: " ++ (partOne text)
  runTestTT partTwoTests
  putStrLn $ "Part two: " ++ (partTwo text)

partOneTests = TestList [
    TestCase (assertEqual "1.1" "2" (partOne "\"\"")),
    TestCase (assertEqual "1.2" "2" (partOne "\"abc\"")),
    TestCase (assertEqual "1.3" "3" (partOne "\"aaa\\\"aaa\"")),
    TestCase (assertEqual "1.4" "5" (partOne "\"\\x27\"")),
    TestCase (assertEqual "1.5" "12" (partOne "\"\"\n\"abc\"\n\"aaa\\\"aaa\"\n\"\\x27\"")),
    TestCase (assertEqual "1.6" (23, 11) (countChars "\"\"\n\"abc\"\n\"aaa\\\"aaa\"\n\"\\x27\""))]

partOne :: String -> String
partOne s = let (tot, chars) = countChars s in
            show $ tot - chars

countChars :: String -> (Int, Int)
countChars s = countChars' s 0 0

countChars' :: String -> Int -> Int -> (Int, Int)
countChars' ('\n' : xs) t c = countChars' xs t c
countChars' ('\"' : xs) t c = countChars' xs (t + 1) c
countChars' ('\\' : '\"' : xs) t c = countChars' xs (t + 2) (c + 1)
countChars' ('\\' : '\\' : xs) t c = countChars' xs (t + 2) (c + 1)
countChars' ('\\' : 'x' : x1 : x2 : xs) t c = countChars' xs (t + 4) (c + 1)
countChars' (_ : xs) t c = countChars' xs (t + 1) (c + 1)
countChars' [] t c = (t, c)

-- These test cases are a pain to type, so I won't add them all.
partTwoTests = TestList [
  TestCase (assertEqual "2.1" "\"\\\"\\\"\"" (encode "\"\"")),
  TestCase (assertEqual "2.2" 4 (countEncodeDifference "\"\""))]

partTwo :: String -> String 
partTwo s = show $ sum $ map countEncodeDifference (lines s)

countEncodeDifference :: String -> Int
countEncodeDifference s = (length . encode $ s) - (length s)

encode :: String -> String
encode s = '\"' : (foldr acc "\"" s)

acc :: Char -> String -> String
acc '\\' s = '\\' : '\\' : s
acc '\"' s = '\\' : '\"' : s
acc x s = x : s
