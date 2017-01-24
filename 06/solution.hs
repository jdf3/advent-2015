-- There's a thunk-leak somewhere that I can't be bothered to debug right now.
-- So, compile with -rtsopts; run with +RTS -K1e8
-- If I was doing this again, I wouldn't want this mutable array to run in the
-- IO monad, opting instead for runSTArray.

{-# LANGUAGE FlexibleContexts #-}

import Test.HUnit
import Data.Array.IO

main = do
  runTestTT tests
  text <- readFile "input.txt"
  let instructions = parseInstructions text
  lights <- newArray ((0, 0), (999, 999)) False
  runCommands instructions boolOp lights
  flat <- mapM (readArray lights) [(x, y) | x <- [0..999], y <- [0..999]]
  print $ "Part one: " ++ (show . length $ filter id flat)

  lightsBrightness <- newArray ((0, 0), (999, 999)) 0
  runCommands instructions intOp lightsBrightness
  flat <- mapM (readArray lightsBrightness) [(x, y) | x <- [0..999], y <- [0..999]]
  print $ "Part two: " ++ (show $ sum flat)

tests = TestList [partOneTests, partTwoTests]

partOneTests = TestList [
  TestCase (assertEqual "parseInstructions"
    [(Off, (1, 1), (2, 2)),
     (On, (13,13), (31, 31)),
     (Toggle, (123, 321), (321, 123))]
    (parseInstructions "turn off 1,1 through 2,2\nturn on 13,13 through 31,31\ntoggle 123,321 through 321,123"))]

partTwoTests = TestList [] 

type LightArray a = IOUArray (Int, Int) a

data Op = On | Off | Toggle deriving (Show, Eq)

parseInstructions :: String -> [(Op, (Int, Int), (Int, Int))]
parseInstructions s = map (parseLine . words) $ lines s

parseLine :: [String] -> (Op, (Int, Int), (Int, Int))
parseLine ("turn" : "off" : rest) =
  (Off, coords . head $ rest, coords . last $ rest)
parseLine ("turn" : "on" : rest) =
  (On, coords . head $ rest, coords . last $ rest)
parseLine ("toggle" : rest) =
  (Toggle, coords . head $ rest, coords . last $ rest)

coords :: String -> (Int, Int)
coords s =
  let (x, y) = break (== ',') s in
  (read x, read $ tail y)

boolOp :: Op -> (Bool -> Bool)
boolOp On = const True
boolOp Off = const False
boolOp Toggle = not

runCommands :: MArray IOUArray a IO =>
               [(Op, (Int, Int), (Int, Int))] -> (Op -> (a -> a)) -> LightArray a -> IO ()
runCommands commands mutator array =
  mapM_ (runCommand array mutator) commands

runCommand :: MArray IOUArray a IO =>
            LightArray a -> (Op -> (a -> a)) -> (Op, (Int, Int), (Int, Int)) -> IO ()
runCommand array mutator (op, (x1, y1), (x2, y2)) =
  mapM_ lightOp [(x, y) | x <- [x1 `min` x2..x1 `max` x2],
                          y <- [y1 `min` y2..y1 `max` y2]]
  where
    lightOp coord = do
      light <- readArray array coord
      writeArray array coord (mutator op $ light)
      return ()

intOp :: Op -> (Int -> Int)
intOp On x = x + 1
intOp Off x = (x - 1) `max` 0
intOp Toggle x = x + 2
