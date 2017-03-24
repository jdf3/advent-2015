import Control.Monad (replicateM)
import Data.List
import Test.HUnit

main = do
  runTestTT partOneTests
  putStrLn $ "Part one: " ++ (show $ snd $ partOne 71 10)

partOneTests = TestList [ 
  [[1,1,1],[1,1,2],[1,2,1],[1,2,2],[2,1,1],[2,1,2],[2,2,1],[2,2,2]] ~=? replicateM 3 [1..2],
  True ~=? killsboss' Player 14 8 10 0 250 0 0 0 [Recharge, Shield, Drain, Poison, Missile]
 ]

data Spell = Missile | Drain | Shield | Poison | Recharge deriving (Show, Eq)
data Turn = Player | Boss deriving Eq

-- hp, damage to lowest amount of mana needed
partOne :: Int -> Int -> (Int, [Spell])
partOne hp damage = (totalcost rotation, rotation)
  where rotation = head $ filter (killsboss hp damage) $ sortBy (\x y -> compare (totalcost x) (totalcost y)) $ replicateM 11 [Missile, Drain, Shield, Poison, Recharge]

-- this rapidly becomes a mess, because of a need to carry around a bunch of
-- state. would this be a place for a monad, if i was more comfortable with
-- them?
killsboss :: Int -> Int -> [Spell] -> Bool
killsboss hp damage spells = killsboss' Player hp damage 50 0 500 0 0 0 spells

killsboss' :: Turn -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Spell] -> Bool
killsboss' _ 0 _ _ _ _ _ _ _ _  = True
killsboss' _ _ _ 0 _ _ _ _ _ _  = False
killsboss' turn bhp bdmg php parmor pmana shieldEff poisonEff rechargeEff spells = 
  simul turn newbhp bdmg php newparmor newpmana (max 0 (shieldEff-1)) (max 0 (poisonEff-1)) (max 0 (rechargeEff-1)) spells
  where newbhp = (max 0 (bhp - (if poisonEff > 0 then 3 else 0)))
        newpmana = pmana + (if rechargeEff > 0 then 101 else 0)
        newparmor = if rechargeEff > 0 then 7 else 0

simul turn bhp bdmg php parm pmana seff peff reff spells
  | bhp == 0 = True
  | php == 0 = False
  | (turn == Player) && (null spells) = False
  | turn == Player = case head spells of
                     Missile -> if pmana < (cost Missile) then False
                                else killsboss' Boss (max 0 (bhp-4)) bdmg php parm (pmana - (cost Missile)) seff peff reff (tail spells)
                     Drain -> if pmana < (cost Drain) then False
                              else killsboss' Boss (max 0 (bhp-2)) bdmg (php+2) parm (pmana - (cost Drain)) seff peff reff (tail spells)
                     Shield -> if pmana < (cost Shield) then False
                               else killsboss' Boss bhp bdmg php parm (pmana - (cost Shield)) 6 peff reff (tail spells)
                     Poison -> if pmana < (cost Poison) then False
                               else killsboss' Boss bhp bdmg php parm (pmana - (cost Poison)) seff 6 reff (tail spells)
                     Recharge -> if pmana < (cost Recharge) then False
                                 else killsboss' Boss bhp bdmg php parm (pmana - (cost Recharge)) seff peff 5 (tail spells)
  | turn == Boss   = killsboss' Player bhp bdmg (max 0 (php-(max 1 (bdmg-parm)))) parm pmana seff peff reff spells
  | otherwise      = error "bad data"

totalcost :: [Spell] -> Int
totalcost xs = sum $ map cost xs

cost :: Spell -> Int
cost Missile = 53
cost Drain = 73
cost Shield = 113
cost Poison = 173
cost Recharge = 229
