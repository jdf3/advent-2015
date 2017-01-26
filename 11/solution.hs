import Test.HUnit
import Data.Char
import Data.List

input :: String
input = "hxbxwxba"

main = do
  runTestTT partOneTests
  putStrLn $ "Part one: " ++ (show . nextvalidpw $ input)

partOneTests = TestList [
  False ~=? (pwvalid "hijklmmn"),
  False ~=? (pwvalid "abbceffg"),
  False ~=? (pwvalid "abbcegjk"),
  "abcdffaa" ~=? (nextvalidpw "abcdefgh"),
  "ghjaabcc" ~=? (nextvalidpw "ghjaabcc")]

nextvalidpw :: String -> String
nextvalidpw p = head . filter pwvalid $ iterate nextpw p

nextpw :: String -> String
nextpw s = reverse . nextpwrev . reverse $ s

nextpwrev :: String -> String
nextpwrev (x:xs)
  | x == 'z' = 'a' : (nextpwrev xs)
  | otherwise = chr (ord x + 1) : xs

pwvalid :: String -> Bool
pwvalid s = (not $ 'i' `elem` s) &&
            (not $ 'o' `elem` s) &&
            (not $ 'l' `elem` s) &&
            (cont3asc s) &&
            (cont2pair s)

cont3asc :: String -> Bool
cont3asc (a : b : c : xs)
  | ord a + 1 == ord b && ord b + 1 == ord c = True
  | otherwise = cont3asc (b : c : xs)
cont3asc _ = False

cont2pair :: String -> Bool
cont2pair s = (length . filter (\x -> (length x) > 1) $ group s) > 1
