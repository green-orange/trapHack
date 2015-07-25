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
import Data.Functor ((<$>))

levelM :: String -> Int
levelM "Bat"             = 1
levelM "Homunculus"      = 1
levelM "Beetle"          = 3
levelM "Ivy"             = 3
levelM "Accelerator"     = 5
levelM "Floating eye"    = 6
levelM "Hunter"          = 7
levelM "Troll"           = 6
levelM "Worm"            = 5
levelM "Red dragon"      = 10
levelM "Green dragon"    = 10
levelM "White dragon"    = 10
levelM "Forgotten beast" = 15
levelM "Spider"          = 6
levelM "Soldier"         = 9
levelM "Umber hulk"      = 7
levelM "Tree"            = 2
levelM "Bot"             = 2
levelM _                 = 0

nOTsOLDIERS, nOTeNEMIES :: [String]

nOTsOLDIERS = nOTeNEMIES ++ ["Bat", "Ivy", "Worm"]
isSoldier :: Monster -> Bool
isSoldier mon = not $ isPlayer mon || elem (name mon) nOTsOLDIERS

nOTeNEMIES = ["You", "Dummy", "Garbage collector", "Rock", "Tail", "Golem",
	"Tree"]
isEnemy :: Monster -> Bool
isEnemy mon = not $ isPlayer mon || elem (name mon) nOTeNEMIES

nOcORPSES :: [String]
nOcORPSES = ["Ivy", "Bot", "Tree", "Tail", "Dummy", "Rock"]

alive :: Monster -> Bool
alive mon = isJust (temp mon' !! fromEnum Nutrition) && hasPart bODY mon' 
	&& hasPart hEAD mon' || hasPart mAIN mon' where
	mon' = mon {parts = filter ((>0) . hp) $ parts mon}
	
regPart :: Part -> Part
regPart part = heal (regRate part) part

regMonster :: Monster -> Monster
regMonster mon = mon {parts = regPart <$> parts mon}

regFirst :: World -> World
regFirst w = changeMon newMon w where
	mon = getFirst w
	newMon = case temp mon !! fromEnum Poison of
		Nothing -> regMonster mon
		Just n -> changeTemp Poison (Just $ max 0 $ (-) n 
			$ res mon !! fromEnum Poison') mon

msgCleanParts :: Monster -> [(String, Int)]
msgCleanParts mon = foldr (:) [] $ filter ((/="") . fst) $ (\x -> (lostMsg (name mon) 
	$ partToStr $ kind x, color)) <$> filter (not . aliveP) (parts mon) where
	color = 
		if name mon == "You"
		then rEDiNVERSE
		else gREEN
		
killFirst :: World -> World
killFirst w = changeMon mon w where
	mon = (getFirst w) {parts = [getMain 0 0 0]}
	
canWalk :: Monster -> Bool
canWalk m = name m `notElem` 
	["Rock", "Ivy", "Tail", "Worm", "Dummy", "Golem", "Tree"]

randTemp :: Temp -> (Int, Int) -> (Monster, StdGen) -> (Monster, StdGen)
randTemp temp' bounds (mon, g) = (newMon, g') where
	(duration, g') = randomR bounds g
	newMon = setMaxTemp temp' (Just duration) mon

level4XP :: String -> Int
level4XP "Ivy"  = 0
level4XP "Tree" = 0
level4XP str = levelM str

xpUp :: StdGen -> Monster -> Monster -> (Monster, Int, StdGen)
xpUp g mon killed = (newMon, lvls, newGen) where
	newXp = xp mon + xp killed + level4XP (name killed)
	lvls = intLog newXp - intLog (xp mon)
	(newMon, newGen) = foldr ($) (mon {xp = newXp}, g)
		$ replicate lvls levelUp

levelUp :: (Monster, StdGen) -> (Monster, StdGen)
levelUp (mon, g) = (mon {parts = newParts}, g') where
	(newParts, g') = levelUpParts g $ parts mon

levelUpParts :: StdGen -> [Part] -> ([Part], StdGen)
levelUpParts g [] = ([], g)
levelUpParts g (p:ps) = (headPart : tailParts, g'') where
	(tailParts, g') = levelUpParts g ps
	n :: Int
	(n, g'') = randomR (0, 5) g'
	headPart = p {hp = hp p + n, maxhp = maxhp p + n}

corpseFromMon :: Monster -> Object
corpseFromMon mon = Food {title = title', nutrition = nutr, 
	weight' = wei, rotRate = 1, rotTime = 800} where
	title' = "corpse of the " ++ name mon
	wei = 5 * sum (maxhp <$> parts mon)
	nutr = sum $ hp <$> parts mon
	
corpseFromPart :: Monster -> Part -> Object
corpseFromPart mon part = Food {title = title', nutrition = nutr, 
	weight' = wei, rotRate = 1, 
	rotTime = if kind part == mAIN then 0 else 500} where
	title' = partToStr (kind part) ++ " of the " ++ name mon
	wei = 5 * maxhp part
	nutr = maxhp part `div` 2

startTemps :: Int -> [Maybe Int]
startTemps n = Just n : tail (const Nothing <$> (getAll :: [Temp]))

intLog :: Int -> Int
intLog = floor . flip (/) (log 2) . log . (fromIntegral :: Int -> Float)

