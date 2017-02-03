import Data.List
import qualified Data.Map as M
import Data.String.Utils (join, split)

import Test.HUnit

main = do 
  runTestTT partOneTests
  text <- readFile "input.txt"
  putStrLn $ "Part one: " ++ (show $ countMolecules (getRules text) (getMolecule text))

partOneTests = TestList [
  4 ~=? (countMolecules [("H", "HO"), ("H", "OH"), ("O", "HH")] "HOH")]

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
