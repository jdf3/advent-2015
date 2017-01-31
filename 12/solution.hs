{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import Data.Aeson
import Data.Char
import Data.Scientific

main = do
  runTestTT partonetests
  text <- readFile "input.txt"
  putStrLn $ "Part one: " ++ (show . partone $ text)
  putStrLn $ "Part two: " ++ (show . parttwo $ text)

partonetests = TestList [
  6 ~=? (partone "[1,2,3]"),
  6 ~=? (partone "{\"a\":2,\"b\":4}"),
  0 ~=? (partone "{\"a\":[-1,1]}"),
  0 ~=? (partone "[-1,{\"a\":1}]"),
  0 ~=? (partone "[]"),
  0 ~=? (partone "{}")]

-- I could rewrite this to use the parser from part two, but that's a tragic
-- waste of time.
partone :: String -> Int
partone s = sumtext 0 "" s

-- Assumption: no numbers like "-8-3"
sumtext :: Int -> String -> String -> Int
sumtext tot seg (x : xs)
  | x == '-' || isDigit x =
      sumtext tot (seg ++ [x]) xs
  | (not $ null $ seg) && (seg /= "-") =
      sumtext (tot + read seg) "" xs
  | otherwise =
      sumtext tot seg xs
sumtext tot seg _
  | (not $ null $ seg) && (seg /= "-") =
      tot + read seg
  | otherwise =
      tot

parttwo :: String -> Int
parttwo s = countignoringred $ fromJust $ decode $ B.pack s

-- Assumption: nothing but integer numbers
countignoringred :: Value -> Int
countignoringred (Object m)
  | String "red" `elem` M.elems m = 0
  | otherwise = sum $ map countignoringred $ M.elems m
countignoringred (Array a) = sum $ map countignoringred $ V.toList a
countignoringred (Number n) = fromJust $ toBoundedInteger n
countignoringred _ = 0

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust _ = error "unsafe"
