import Test.HUnit
import qualified Data.Map as M
import Data.List

main = do
  runTestTT partOneTests
  text <- readFile "input.txt"
  putStrLn $ "Part one: " ++ (show . optimalseats $ text)

partOneTests = TestList [
  330 ~=? optimalseats "Alice would gain 54 happiness units by sitting next to Bob.\nAlice would lose 79 happiness units by sitting next to Carol.\nAlice would lose 2 happiness units by sitting next to David.\nBob would gain 83 happiness units by sitting next to Alice.\nBob would lose 7 happiness units by sitting next to Carol.\nBob would lose 63 happiness units by sitting next to David.\nCarol would lose 62 happiness units by sitting next to Alice.\nCarol would gain 60 happiness units by sitting next to Bob.\nCarol would gain 55 happiness units by sitting next to David.\nDavid would gain 46 happiness units by sitting next to Alice.\nDavid would lose 7 happiness units by sitting next to Bob.\nDavid would gain 41 happiness units by sitting next to Carol."]

optimalseats :: String -> Int
optimalseats s = maximum $ map (cost pairs) combinations
  where pairs = buildmap s
        keys = nub $ map fst $ M.keys pairs
        combinations = map (\xs -> (head keys : xs)) $ permutations $ tail keys

cost :: (M.Map (String, String) Int) -> [String] -> Int
cost m (x:xs) = (m M.! (x, l)) + (m M.! (l, x)) + (cost' m (x:xs))
  where l = last xs

cost' :: (M.Map (String, String) Int) -> [String] -> Int
cost' m (x1:x2:xs) = (m M.! (x1, x2)) + (m M.! (x2, x1)) + (cost' m (x2:xs))
cost' m _ = 0

buildmap :: String -> (M.Map (String, String) Int)
buildmap s = M.fromList $ map readline $ lines s

readline :: String -> ((String, String), Int)
readline s = parseline $ words s

parseline :: [String] -> ((String, String), Int)
parseline [n1, "would", "gain", amount, _, _, _, _, _, _, n2] =
  ((n1, init n2), read amount)
parseline [n1, "would", "lose", amount, _, _, _, _, _, _, n2] =
  ((n1, init n2), - (read amount))
parseline _ = error "bad parse"
