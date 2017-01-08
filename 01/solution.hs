main = do
  text <- readFile "input.txt"
  putStrLn ("Part one: " ++ (show (sumFloors text)))
  putStrLn ("Part one: " ++ (show (firstFloorNegative text)))

sumFloors :: [Char] -> Int
sumFloors [] = 0
sumFloors (x:xs) = if x == ')' then -1 + sumFloors xs
                   else if x == '(' then 1 + sumFloors xs
                   else sumFloors xs

firstFloorNegative :: [Char] -> Int
firstFloorNegative xs = _firstFloorNegative 0 0 xs

_firstFloorNegative :: Int -> Int -> [Char] -> Int
_firstFloorNegative pos sum [] = -1
_firstFloorNegative pos sum (x:xs) =
  if x == ')' then
    if sum - 1 == -1 then
      pos + 1
    else
      _firstFloorNegative (pos + 1) (sum - 1) xs
  else if x == '(' then
    _firstFloorNegative (pos + 1) (sum + 1) xs
  else
    _firstFloorNegative (pos + 1) sum xs
