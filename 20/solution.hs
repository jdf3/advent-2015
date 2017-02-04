import Data.List

main = do
  putStrLn $ "Part one: " ++ (show $ head $ filter (\x -> (presents x) > 34000000) [1..])
  putStrLn $ "Part two: " ++ (show $ head $ filter (\x -> (presents' x) > 34000000) [1..])

factors :: Int -> [Int]
factors n = [i | i <- [1..(floor $ sqrt $ fromIntegral n)],
                 (d,0) <- [divMod n i],
                 i <- [i] ++ [d | d > i] ]

presents :: Int -> Int
presents i = 10 * (sum $ factors i)

factors' :: Int -> [Int]
factors' i = filter (\f -> (i `div` f) <= 50) $ factors i

presents' :: Int -> Int
presents' i = 11 * (sum $ factors' i)
