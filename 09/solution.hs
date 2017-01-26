import Test.HUnit
import Data.List
import qualified Data.Map as M

main = do
  runTestTT partOneTests
  text <- readFile "input.txt"
  putStrLn $ "Part one: " ++ (show $ partOne text)

partOneTests = TestList [
  TestCase (assertEqual "1.1" 605 (partOne "London to Dublin = 464\nLondon to Belfast = 518\nDublin to Belfast = 141"))]

partOne :: String -> Int
partOne s = let l = readData s in
            let m = readMap l in
            let locs = readLocs l in
            let options = permutations locs in
            minimum $ map (pathCost m) options

pathCost :: (M.Map (String, String) Int) -> [String] -> Int
pathCost m (loc1 : loc2 : xs) = (m M.! (loc1, loc2)) + (pathCost m (loc2 : xs))
pathCost _ _ = 0

readMap :: [((String, String), Int)] -> (M.Map (String, String) Int)
readMap l = M.fromList l

readLocs :: [((String, String), Int)] -> [String]
readLocs l = nub ((map (fst . fst) l) ++ (map (snd . fst) l))

-- Returns the list of all locations, and a map mapping each pair of locations (symmetric inclusion) to the distance.
readData :: String -> [((String, String), Int)]
readData s = foldl (++) [] $ map readLine $ lines s

-- Returns the two locations listed, and their distance.
readLine :: String -> [((String, String), Int)]
readLine s = readWords $ words s

readWords :: [String] -> [((String, String), Int)]
readWords [loc1, "to", loc2, "=", distance] = [((loc1, loc2), d), ((loc2, loc1), d)]
  where d = read distance
readWords _ = error "Bad data"
