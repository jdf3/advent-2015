-- With some credit to Bart Massey's solution for inspiration

import Control.Monad (replicateM)
import Data.List
import Test.HUnit

main = do
  runTestTT partOneTests
  putStrLn $ "Part one: " ++ (show $ partOne 71 10)

partOneTests = TestList [ 
 (Stats Self 2 0 0 24 226, Stats Boss 0 8 0 0 0) ~=? endstate (Stats Self 10 0 0 250 0) (Stats Boss 13 8 0 0 0),
 (Stats Self 1 0 0 114 641, Stats Boss 0 8 0 0 0) ~=? endstate (Stats Self 10 0 0 250 0) (Stats Boss 14 8 0 0 0)
]

cost :: Spell -> Int
cost Missile = 53
cost Drain = 73
cost Shield = 113
cost Poison = 173
cost Recharge = 229

type Effect = (Stats, Stats) -> (Stats, Stats)

data Player = Self | Boss deriving (Ord, Eq, Show)

data Stats = Stats {
  statsType :: Player
  statsHP :: Int,
  statsDamage :: Int,
  statsArmor :: Int,
  statsMana :: Int,
  statsManaSpent :: Int
} deriving (Ord, Eq, Show)

endstate :: Stats -> Stats -> [Spell] -> (Stats, Stats)

