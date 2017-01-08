import Text.Regex.Posix
import Test.HUnit

main = do
  runTestTT tests
  text <- readFile "input.txt"
  putStrLn . partOne $ text
  putStrLn . partTwo $ text

tests = TestList [wrappingPaperOne, wrappingPaperTwo, ribbonOne, ribbonTwo]

wrappingPaperOne = TestCase (assertEqual "wrapping paper 1" 58 (wrappingPaperNeeded (2, 3, 4)))
wrappingPaperTwo = TestCase (assertEqual "wrapping paper 2" 43 (wrappingPaperNeeded (1, 1, 10)))

ribbonOne = TestCase (assertEqual "ribbon 1" 34 (ribbonNeeded (2, 3, 4)))
ribbonTwo = TestCase (assertEqual "ribbon 2" 14 (ribbonNeeded (1, 1, 10)))
  
partOne :: String -> String
partOne t = "Part one: " ++ show total
  where total = totalSurfaceArea . getMatches $ t

getMatches :: String -> [[String]]
getMatches text = text =~ "([0-9]+)x([0-9]+)x([0-9]+)" :: [[String]]

totalSurfaceArea :: [[String]] -> Int
totalSurfaceArea xs =  sum $ map (wrappingPaperNeeded . dimensions) xs

dimensions :: [String] -> (Int, Int, Int)
dimensions xs = (read (xs !! 1), read (xs !! 2), read (xs !! 3))

wrappingPaperNeeded :: (Int, Int, Int) -> Int
wrappingPaperNeeded d = 2 * (sum . areas $ d) + (minimum . areas $ d)

areas :: (Int, Int, Int) -> [Int]
areas (l, w, h) = [l * w, w * h, h * l]

partTwo :: String -> String
partTwo t = "Part two: " ++ show total
  where total = totalRibbonNeeded . getMatches $ t

totalRibbonNeeded :: [[String]] -> Int
totalRibbonNeeded xs = sum $ map (ribbonNeeded . dimensions) xs

ribbonNeeded :: (Int, Int, Int) -> Int
ribbonNeeded dim = volume dim + smallestPerimeter dim

volume :: (Int, Int, Int) -> Int
volume (l, w, h) = l * w * h

smallestPerimeter :: (Int, Int, Int) -> Int
smallestPerimeter dim = minimum . perimeters $ dim

perimeters :: (Int, Int, Int) -> [Int]
perimeters (l, w, h) = [(l + w) * 2, (w + h) * 2, (h + l) *2]

