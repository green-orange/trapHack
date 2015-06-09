module Utils4AI where

import Data
import Utils4all

needToBeHealedM :: Monster -> Bool
needToBeHealedM mon =
	foldl (||) False $ map (\x -> kind x == bODY && needToBeHealed x) $ parts mon

needToBeHealed :: Part -> Bool
needToBeHealed part = 3 * (hp part) < maxhp part

canBeHealed :: Monster -> Bool
canBeHealed mon = foldl (||) False $ map (isHealing . second) $ inv mon

isHealing :: Object -> Bool
isHealing obj = title obj == "potion of healing"

healAI :: World -> Char
healAI world = first $ head $ filter (isHealing . second) $ inv $ getFirst world

canZapToAttack :: Monster -> Bool
canZapToAttack mon = foldl (||) False $ map (isAttackWand . second) $ inv mon

canFire :: Monster -> Bool
canFire mon = haveMissile mon && haveLauncher mon

haveMissile :: Monster -> Bool
haveMissile mon = foldl (||) False $ map (isMissile . second) $ inv mon

haveLauncher :: Monster -> Bool
haveLauncher mon = foldl (||) False $ map (isLauncher . second) $ inv mon

isAttackWand :: Object -> Bool
isAttackWand obj = isWand obj && charge obj > 0 && title obj == "wand of striking"

zapAI :: World -> Char
zapAI world = first $ head $ filter (isAttackWand . second) $ inv $ getFirst world

missileAI :: World -> Char
missileAI world = first $ head $ filter (isMissile . second) $ inv $ getFirst world

launcherAI :: World -> Char
launcherAI world = first $ head $ filter (isLauncher . second) $ inv $ getFirst world

isOnLine :: Int -> Int -> Int -> Int -> Int -> Bool
isOnLine d x1 y1 x2 y2 = abs (x1 - x2) <= d && abs (y1 - y2) <= d &&
	(x1 == x2 || y1 == y2 || x1 - y1 == x2 - y1 || x1 + y1 == x2 + y2)


