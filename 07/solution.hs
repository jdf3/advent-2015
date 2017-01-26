-- Strategy: let's not bother with the mutable hashtables for now, and just use
-- Data.Map.

import Test.HUnit
import qualified Data.Map as Map
import Data.Bits
import Data.Word
import Text.Read

main = do
  runTestTT partOneTests
  putStrLn ""
  text <- readFile "input.txt"
  putStrLn $ "Part one: " ++ (partOne text)
  putStrLn $ "Part two: " ++ (partTwo text)

partOneTests = TestList [
  TestCase (assertEqual "partOne" "65079" (partOne "123 -> x\n456 -> y\nx AND y -> d\nx OR y -> e\nx LSHIFT 2 -> f\ny RSHIFT 2 -> g\nNOT x -> h\nNOT y -> a"))]

partTwoTests = TestList [] 

partOne :: String -> String
partOne s = show $ snd $ getValue s "a" 

partTwo :: String -> String
partTwo s = let a = partOne s in
            show $ snd $ getValue (s ++ a ++ " -> b") "a"

getValue s reg = getValue' (Map.fromList $ parseInstructions s) reg

data Instruction = UNARY (Word16 -> Word16) String
                 | BINARY (Word16 -> Word16 -> Word16) String String
                 | VALUE Word16

getValue' :: (Map.Map String Instruction) -> String -> (Map.Map String Instruction, Word16)
getValue' m reg =
  if (readMaybe reg :: Maybe Word16) /= Nothing then (m, read reg :: Word16)
  else case m Map.! reg of
    (VALUE x) -> (m, x)
    (BINARY op x y) -> 
      let (m1, x1) = getValue' m x in
      let (m2, y2) = getValue' m1 y in
      let value = x1 `op` y2 in
      (Map.insert reg (VALUE value) m2, value)
    (UNARY op x) ->
      let (m', x') = getValue' m x in
      let value = op x' in
      (Map.insert reg (VALUE value) m', value)

parseInstructions :: String -> [(String, Instruction)]
parseInstructions s = map (parseLine . words) $ lines s

parseLine :: [String] -> (String, Instruction)
parseLine [first, "AND", second, "->", last] =
  (last, BINARY (.&.) first second)
parseLine [first, "OR", second, "->", last] =
  (last, BINARY (.|.) first second)
parseLine [first, "LSHIFT", second, "->", last] =
  (last, BINARY blshift first second)
parseLine [first, "RSHIFT", second, "->", last] =
  (last, BINARY brshift first second)
parseLine ["NOT", argument, "->", last] =
  (last, UNARY complement argument)
parseLine [argument, "->", last]
  | (readMaybe argument :: Maybe Word16) /= Nothing = (last, VALUE (read argument :: Word16))
  | otherwise = (last, UNARY id argument)
parseLine bad = error $ "Bad instruction: " ++ (show bad)

blshift :: Word16 -> Word16 -> Word16
blshift value s = value `shift` (fromIntegral s)

brshift :: Word16 -> Word16 -> Word16
brshift value s = value `shift` (- (fromIntegral s))
