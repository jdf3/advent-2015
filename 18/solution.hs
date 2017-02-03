import Test.HUnit
import qualified Data.Map as M
import Data.List

main = do
  runTestTT partOneTests
  text <- readFile "input.txt"
  putStrLn $ "Part one: " ++ (show $ countLights partOneIter text 100)
  runTestTT partTwoTests
  putStrLn $ "Part two: " ++ (show $ countLights partTwoIter text 100)

pOneTestBoard = ".#.#.#\n\
            \...##.\n\
            \#....#\n\
            \..#...\n\
            \#.#..#\n\
            \####.."

partOneTests = TestList [
  11 ~=? (countLights partOneIter pOneTestBoard 1),
  8 ~=? (countLights partOneIter pOneTestBoard 2),
  4 ~=? (countLights partOneIter pOneTestBoard 3),
  4 ~=? (countLights partOneIter pOneTestBoard 4)]

pTwoTestBoard = "##.#.#\
                \...##.\n\
                \#....#\n\
                \..#...\n\
                \#.#..#\n\
                \####.#"

partTwoTests = TestList [
  17 ~=? (countLights partTwoIter pTwoTestBoard 5)]

data Light = On | Off deriving Eq

countLights :: (M.Map (Int, Int) Light -> M.Map (Int, Int) Light) -> String -> Int -> Int
countLights iter s i =
  let lights = parseLights s
      finish = (iterate iter lights) !! i
  in length $ filter (== On) $ M.elems finish

partOneIter :: M.Map (Int, Int) Light -> M.Map (Int, Int) Light
partOneIter = iterLights

partTwoIter :: M.Map (Int, Int) Light -> M.Map (Int, Int) Light
partTwoIter m = fixCorners $ iterLights m

fixCorners :: M.Map (Int, Int) Light -> M.Map (Int, Int) Light
fixCorners m =
  let keys = M.keys m
      minx = minimum $ map fst keys
      miny = minimum $ map snd keys
      maxx = maximum $ map fst keys
      maxy = maximum $ map snd keys
      corners = M.fromList [
        ((minx, miny), On),
        ((minx, maxy), On),
        ((maxx, miny), On),
        ((maxx, maxy), On)]
  in corners `M.union` m

iterLights :: M.Map (Int, Int) Light -> M.Map (Int, Int) Light
iterLights m = M.fromList $ map (\coord -> (coord, iterLight m coord)) $ M.keys m

iterLight :: M.Map (Int, Int) Light -> (Int, Int) -> Light
iterLight m (x, y) =
  let light = m M.! (x, y)
      ns = onNeighbors (x, y) m
  in case (light, ns) of
    (On, 2) -> On
    (On, 3) -> On
    (Off, 3) -> On
    _ -> Off

onNeighbors :: (Int, Int) -> M.Map (Int, Int) Light -> Int
onNeighbors coord m = length $ filter (\x -> x == Just On) $ map (flip M.lookup m) $ neighbors coord

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(a, b) | a <- [x-1 .. x+1], b <- [y-1 .. y+1]] \\ [(x, y)]

parseLights :: String  -> M.Map (Int, Int) Light
parseLights s = M.fromList $ getLights s 0 0

getLights :: String -> Int -> Int -> [((Int, Int), Light)]
getLights ('.':xs) x y = ((x, y), Off) : (getLights xs (x+1) y)
getLights ('#':xs) x y = ((x, y), On) : (getLights xs (x+1) y)
getLights ('\n':xs) x y = getLights xs 0 (y+1)
getLights "" _ _ = []
