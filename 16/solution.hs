import Data.List

main = do
  text <- readFile "input.txt"
  putStrLn $ "Part one: " ++ (show $ whichsue text)

correctsue :: [(String, Int)]
correctsue = [
  ("children", 3),
  ("cats", 7),
  ("samoyeds", 2),
  ("pomeranians", 3),
  ("akitas", 0),
  ("vizslas", 0),
  ("goldfish", 5),
  ("trees", 3),
  ("cars", 2),
  ("perfumes", 1)]

whichsue :: String -> Int
whichsue t = number $ head $ filter (suematch correctsue) $ parsesues t

data Sue = Sue {
  number :: Int,
  properties :: [(String, Int)]} deriving (Show, Eq)

suematch :: [(String, Int)] -> Sue -> Bool
suematch m s = null $ (p \\ m)
  where p = properties s

parsesues :: String -> [Sue]
parsesues s = map parsesue $ map words $ lines s
  where
    parsesue :: [String] -> Sue
    parsesue ("Sue" : num : attrs) =
      Sue (read $ init num) (parseattrs attrs)
      where
        parseattrs :: [String] -> [(String, Int)]
        parseattrs (name : count : rest) =
          (strippunc name, read $ strippunc count) : parseattrs rest
        parseattrs (_ : _) = error "bad parse"
        parseattrs _ = []
    parsesue _ = error "bad parse"

strippunc :: String -> String
strippunc = filter (\x -> not $ x `elem` ",:")
