import Data.List

main = do
  text <- readFile "input.txt"
  putStrLn $ "Part one: " ++ (show $ maxscore text)
  putStrLn $ "Part two: " ++ (show $ maxscore' text)

maxscore :: String -> Int
maxscore text = maximum $ map (score ingredients) $ combs
  where
    ingredients = parseattrs text False
    combs = getcombs 100 (length ingredients)

maxscore' :: String -> Int
maxscore' text = maximum $ map (score' ingredients) $ combs
  where
    ingredients = parseattrs text True
    combs = getcombs 100 (length ingredients)

-- given a list of ingredient attributes and a combination, score it
score :: [[Int]] -> [Int] -> Int
score xs ys =
  let amts = amounts xs ys in
  if minimum amts < 0 then 0
  else product amts

score' :: [[Int]] -> [Int] -> Int
score' xs ys = 
  let amts = amounts xs ys in
  if minimum amts < 0 then 0
  else if last amts /= 500 then 0
  else product $ init amts
    
-- given a list of ingredient attributes, gets the amounts of each aytribute
amounts :: [[Int]] -> [Int] -> [Int]
amounts (x:xs) (y:ys) = zipWith (+) (map (y *) x) (amounts xs ys)
amounts _ _ = repeat 0

parseattrs :: String -> Bool -> [[Int]]
parseattrs s cals = map (parseline cals) $ map words $ lines s
  where
    parseline :: Bool -> [String] -> [Int] 
    parseline True [name, "capacity", capacity, "durability", durability, "flavor", flavor, "texture", texture, "calories", calories] =
      [read $ init capacity, read $ init durability, read $ init flavor, read $ init texture, read calories]
    parseline False [name, "capacity", capacity, "durability", durability, "flavor", flavor, "texture", texture, "calories", calories] =
      [read $ init capacity, read $ init durability, read $ init flavor, read $ init texture]
    parseline _ _ = error "bad parse"

-- Lists of Ints of length count which sum to total
getcombs :: Int -> Int -> [[Int]]
getcombs total count = 
  iterate extend [[]] !! count
  where
    extend :: [[Int]] -> [[Int]]
    extend xs =
      concatMap ext xs
      where
        ext xs = 
          map (:xs) [0 .. total - sum xs]
