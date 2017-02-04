import Control.Applicative (liftA2, liftA3)
import Data.List (sort)

main = do
  text <- readFile "input.txt"
  putStrLn $ "Part one: " ++ (show $ cheapest text)
  putStrLn $ "Part two: " ++ (show $ mostExpensive text)

cheapest :: String -> Int
cheapest boss = fst3 $ head $ filter (wins bossStats) $ sort combs

mostExpensive :: String -> Int
mostExpensive boss = fst3 $ head $ filter (loses bossStats) $ reverse $ sort combs
  where
    loses :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
    loses b h = not $ wins b h 

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- just read from input.txt
bossStats :: (Int, Int, Int)
bossStats = (100, 8, 2)

-- There's probably a smarter implementation of this using modular arithmetic,
-- but this is fine
wins :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
wins (bhp, batk, barm) (_, hatk, harm) =
  if hatk - barm <= 0 then False
  else wins' (bhp, batk, barm) (100, hatk, harm)

wins' :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
wins' (bhp, batk, barm) (hhp, hatk, harm)
  | bhp <= 0 = True
  | hhp <= 0 = False
  | otherwise = wins' (bhp - (hatk - barm), batk, barm) (hhp - (batk - harm), hatk, harm)

combs :: [(Int, Int, Int)]
combs = liftA3 (\t1 t2 t3 -> tupPlus t1 $ tupPlus t2 t3) weapons armors ringCombs

ringCombs :: [(Int, Int, Int)]
ringCombs = liftA2 (tupPlus) rings rings

tupPlus :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
tupPlus (a, b, c) (x, y, z) = (a + x, b + y, c + z)

weapons :: [(Int, Int, Int)]
weapons = [
  (8, 4, 0),
  (10, 5, 0),
  (25, 6, 0),
  (40, 7, 0),
  (74, 8, 0)]

armors :: [(Int, Int, Int)]
armors = [
  (0, 0, 0), -- this counts as "no armor"
  (13, 0, 1),
  (31, 0, 2),
  (53, 0, 3),
  (75, 0, 4),
  (102, 0, 5)]

rings :: [(Int, Int, Int)]
rings = [
  (0, 0, 0),
  (25, 1, 0),
  (50, 2, 0),
  (100, 3, 0),
  (20, 0, 1),
  (40, 0, 2),
  (80, 0, 3)]
