module Utils.Monsters where

import Data.Const
import Data.World
import Data.Monster
import Data.Define
import Utils.Changes
import Utils.HealDamage
import Monsters.Parts
import IO.Messages
import IO.Colors

import System.Random (StdGen, randomR)
import Data.Maybe (isJust)

levelM :: String -> Int
levelM "Bat"             = 1
levelM "Homunculus"      = 3
levelM "Beetle"          = 5
levelM "Ivy"             = 3
levelM "Accelerator"     = 6
levelM "Floating eye"    = 5
levelM "Hunter"          = 7
levelM "Troll"           = 7
levelM "Worm"            = 5
levelM "Red dragon"      = 10
levelM "Green dragon"    = 10
levelM "White dragon"    = 10
levelM "Forgotten beast" = 15
levelM "Spider"          = 8
levelM "Soldier"         = 8
levelM "Umber hulk"      = 9
levelM _                 = 0

nOTsOLDIERS, nOTeNEMIES :: [String]

nOTsOLDIERS = nOTeNEMIES ++ ["Bat", "Ivy", "Worm"]
isSoldier :: Monster -> Bool
isSoldier mon = not $ isPlayer mon || elem (name mon) nOTsOLDIERS

nOTeNEMIES = ["You", "Dummy", "Garbage collector", "Rock", "Tail", "Golem"]
isEnemy :: Monster -> Bool
isEnemy mon = not $ isPlayer mon || elem (name mon) nOTeNEMIES

alive :: Monster -> Bool
alive mon = isJust (temp mon' !! fromEnum Nutrition) && hasPart bODY mon' 
	&& hasPart hEAD mon' || hasPart mAIN mon' where
	mon' = mon {parts = filter ((>0) . hp) $ parts mon}

hasPart :: Int -> Monster -> Bool
hasPart knd mon = 
	any (\x -> kind x == knd) $ parts mon

hasUpperLimb :: Monster -> Bool
hasUpperLimb mon = any isUpperLimb $ parts mon
	
regPart :: Part -> Part
regPart part = heal (regVel part) part

regMonster :: Monster -> Monster
regMonster mon = changeParts (map regPart $ parts mon) mon

regFirst :: World -> World
regFirst w = changeMon newMon w where
	mon = getFirst w
	newMon = case temp mon !! fromEnum Poison of
		Nothing -> regMonster mon
		Just n -> changeTemp Poison (Just $ max 0 $ (-) n 
			$ res mon !! fromEnum Poison') mon

msgCleanParts :: Monster -> [(String, Int)]
msgCleanParts mon = foldr (:) [] $ filter ((/="") . fst) $ map (\x -> (lostMsg (name mon) 
	$ partToStr $ kind x, color)) $ filter (not . aliveP) $ parts mon where
	color = 
		if name mon == "You"
		then rEDiNVERSE
		else gREEN

isFlying :: Monster -> Bool
isFlying = hasPart wING
		
killFirst :: World -> World
killFirst w = changeMon mon w where
	mon = (getFirst w) {parts = [getMain 0 0 0]}
	
canWalk :: Monster -> Bool
canWalk m = name m `notElem` ["Rock", "Ivy", "Tail", "Worm", "Dummy", "Golem"]

randTemp :: Temp -> (Int, Int) -> (Monster, StdGen) -> (Monster, StdGen)
randTemp temp' bounds (mon, g) = (newMon, g') where
	(duration, g') = randomR bounds g
	newMon = setMaxTemp temp' (Just duration) mon

level4XP :: String -> Int
level4XP "Ivy" = 0
level4XP str = levelM str

xpUp :: StdGen -> Monster -> Monster -> (Monster, Int, StdGen)
xpUp g mon killed = (newMon, lvls, newGen) where
	newXp = xp mon + xp killed + level4XP (name killed)
	lvls = intLog newXp - intLog (xp mon)
	(newMon, newGen) = foldr ($) (mon {xp = newXp}, g)
		$ replicate lvls levelUp

levelUp :: (Monster, StdGen) -> (Monster, StdGen)
levelUp (mon, g) = (changeParts newParts mon, g') where
	(newParts, g') = levelUpParts g $ parts mon

levelUpParts :: StdGen -> [Part] -> ([Part], StdGen)
levelUpParts g [] = ([], g)
levelUpParts g (p:ps) = (headPart : tailParts, g'') where
	(tailParts, g') = levelUpParts g ps
	n :: Int
	(n, g'') = randomR (0, 5) g'
	headPart = p {hp = hp p + n, maxhp = maxhp p + n}

corpseFromMon :: Monster -> Object
corpseFromMon mon = Food {title = title', nutrition = nutr, weight' = wei} where
	title' = "corpse of the " ++ name mon
	wei = 5 * sum (map maxhp $ parts mon)
	nutr = sum $ map hp $ parts mon
	
corpseFromPart :: Monster -> Part -> Object
corpseFromPart mon part = Food {title = title', nutrition = nutr, weight' = wei} where
	title' = partToStr (kind part) ++ " of the " ++ name mon
	wei = 5 * maxhp part
	nutr = maxhp part `div` 2

startTemps :: Int -> [Maybe Int]
startTemps n = Just n : tail (map (const Nothing) (getAll :: [Temp]))

intLog :: Int -> Int
intLog = floor . flip (/) (log 2) . log . (fromIntegral :: Int -> Float)

