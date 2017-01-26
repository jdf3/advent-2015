import Test.HUnit
import Data.Char

main = do
  runTestTT partonetests
  text <- readFile "input.txt"
  putStrLn $ "Part one: " ++ (show . partone $ text)

partonetests = TestList [
  6 ~=? (partone "[1,2,3]"),
  6 ~=? (partone "{\"a\":2,\"b\":4}"),
  0 ~=? (partone "{\"a\":[-1,1]}"),
  0 ~=? (partone "[-1,{\"a\":1}]"),
  0 ~=? (partone "[]"),
  0 ~=? (partone "{}")]

-- Sums all the numbers in the file
partone :: String -> Int
partone s = sumtext 0 "" s

-- Assumption: no numbers like "-8-3"
sumtext :: Int -> String -> String -> Int
sumtext tot seg (x : xs)
  | x == '-' || isDigit x =
      sumtext tot (seg ++ [x]) xs
  | (not $ null $ seg) && (seg /= "-") =
      sumtext (tot + read seg) "" xs
  | otherwise =
      sumtext tot seg xs
sumtext tot seg _
  | (not $ null $ seg) && (seg /= "-") =
      tot + read seg
  | otherwise =
      tot
