import Data.List
import qualified Data.Map as M

main = do
  text <- readFile "input.txt"
  putStrLn $ "Part one: " ++ (show $ whichsue text)
  putStrLn $ "Part two: " ++ (show $ whichsue' text)

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

whichsue' :: String -> Int
whichsue' t = number $ head $ filter (suematch' correctsue) $ parsesues t

suematch' :: [(String, Int)] -> Sue -> Bool
suematch' m s = suematch'' (M.fromList m) (properties s)
  where
    suematch'' :: (M.Map String Int) -> [(String, Int)] -> Bool
    suematch'' m ((name, count) : rest)
      | name == "cats" || name == "trees" =
        if count <= (m M.! name) then False
        else suematch'' m rest
      | name == "pomeranians" || name == "goldfish" =
        if count >= (m M.! name) then False
        else suematch'' m rest
      | otherwise =
        if count /= (m M.! name) then False
        else suematch'' m rest
    suematch'' _ _ = True
