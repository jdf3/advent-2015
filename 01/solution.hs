import qualified Data.Text as T

main = do
  text <- readFile "input.txt"
  putStrLn (show (numFloors (T.pack text)))

numFloors :: T.Text -> Int
numFloors t = opens - closes
  where opens = T.count (T.pack "(") t
        closes = T.count (T.pack ")") t
