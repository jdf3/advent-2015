import Data.Char (isLower, isUpper)
import Data.List
import qualified Data.Map as M
import Data.String.Utils (join, split)

import Test.HUnit

main = do 
  runTestTT partOneTests
  text <- readFile "input.txt"
  putStrLn $ "Part one: " ++ (show $ countMolecules (getRules text) (getMolecule text))
  runTestTT partTwoTests
  -- BFS implementation, too slow.
  --putStrLn $ "Part two: " ++ (show $ generateMolecule (getRules text) (getMolecule text))
  putStrLn $ "Part two: " ++ (show $ genMolecule (getMolecule text))

partOneTests = TestList [
  4 ~=? (countMolecules [("H", "HO"), ("H", "OH"), ("O", "HH")] "HOH")]

partTwoTests = TestList [
  3 ~=? (generateMolecule [("e", "H"), ("e", "O"), ("H", "HO"), ("H", "OH"), ("O", "HH")] "HOH"),
  6 ~=? (generateMolecule [("e", "H"), ("e", "O"), ("H", "HO"), ("H", "OH"), ("O", "HH")] "HOHOHO")]

countMolecules :: [(String, String)] -> String -> Int
countMolecules rules base =
  length $ nub $ concatMap (flip replace base) rules

replace :: (String, String) -> String -> [String]
replace (s, t) base = map (join s) $ combs t $ split s base
  where
    combs :: String -> [String] -> [[String]]
    combs r (x:y:rest) =
      ((x ++ r ++ y) : rest) : (map (x:) $ combs r (y : rest))
    combs r _ = []

getRules :: String -> [(String, String)]
getRules s =
  map parse $ map words $ filter (\x -> "=>" `isInfixOf` x) $ lines s
    where
      parse :: [String] -> (String, String)
      parse [one, "=>", two] = (one, two)
      parse _ = error "bad parse"

getMolecule :: String -> String
getMolecule s = last $ lines s

genMolecule :: String -> Int
genMolecule s =
  let atoms = (getAtoms s)
      countAtoms atom = length $ filter (atom ==) (getAtoms s)
  in length atoms - countAtoms "Rn" - countAtoms "Ar" - 2 * countAtoms "Y" - 1

getAtoms :: String -> [String]
getAtoms (x:y:rest)
  | isUpper x && isLower y = ([x] ++ [y]) : (getAtoms rest)
  | isUpper x && isUpper y = ([x]) : (getAtoms (y:rest))
  | otherwise = error "bad molecule"
getAtoms (x:rest)
  | isUpper x = ([x]) : (getAtoms rest)
  | otherwise = error "bad molecule"
getAtoms "" = []

-- BFS, too slow, but is correct
generateMolecule :: [(String, String)] -> String -> Int
generateMolecule rules target = fromJust $ findIndex (target `elem`) $ iterate (iter rules) ["e"]

iter :: [(String, String)] -> [String] -> [String]
iter rules bases = nub $ iter' rules bases
  where
    iter' :: [(String, String)] -> [String] -> [String]
    iter' rules (base:rest) =
      concatMap (flip replace base) rules ++ (iter' rules rest)
    iter' _ _ = []

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust _ = error "unsafe"
