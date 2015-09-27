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

-- | return danger level by monster name
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
levelM "Tree"            = 0
levelM "Bot"             = 2
levelM "Bee"             = 2
levelM _                 = 0

nOTsOLDIERS, nOTeNEMIES :: [String]

-- | names of monsters who doesn't attack you specially
nOTsOLDIERS = nOTeNEMIES ++ ["Homunculus", "Bat", "Ivy", "Worm"]
-- | can this monster attack you specially?
isSoldier :: Monster -> Bool
isSoldier mon = not $ isPlayer mon || elem (name mon) nOTsOLDIERS

-- | names of monsters who doesn't attack you
nOTeNEMIES = ["You", "Dummy", "Garbage collector", "Rock", "Tail", "Golem",
	"Tree"]
-- | can this monster attack you?
isEnemy :: Monster -> Bool
isEnemy mon = not $ isPlayer mon || elem (name mon) nOTeNEMIES

-- | names of monsters who can't leave corpses
nOcORPSES :: [String]
nOcORPSES = ["Ivy", "Bot", "Tree", "Tail", "Dummy", "Rock"]

-- | is this monster alive?
-- monster can die if it has no head ot body and no 'main'
-- or if it was starved (nutrition == 0)
alive :: Monster -> Bool
alive mon = isJust (temp mon' !! fromEnum Nutrition) && hasPart bODY mon' 
	&& hasPart hEAD mon' || hasPart mAIN mon' where
	mon' = mon {parts = filter ((>0) . hp) $ parts mon}

-- | regenerate the part
regPart :: Part -> Part
regPart part = heal (regRate part) part

-- | regenerate all parts of a monster
regMonster :: Monster -> Monster
regMonster mon = mon {parts = regPart <$> parts mon}

-- | regenerate current monster if not poisoned 
regFirst :: World -> World
regFirst w = changeMon newMon w where
	mon = getFirst w
	newMon = case temp mon !! fromEnum Poison of
		Nothing -> regMonster mon
		Just n -> changeTemp Poison (Just $ max 0 $ (-) n 
			$ res mon !! fromEnum Poison') mon

-- | add messages like "Foo lost Bar"
msgCleanParts :: Monster -> [(String, Int)]
msgCleanParts mon = foldr (:) [] $ filter ((/="") . fst) $ (\x -> (lostMsg (name mon) 
	$ partToStr $ kind x, color)) <$> filter (not . aliveP) (parts mon) where
	color = 
		if name mon == "You"
		then rEDiNVERSE
		else gREEN

-- | instakill current monster (used for worm and ivy)
killFirst :: World -> World
killFirst w = changeMon mon w where
	mon = (getFirst w) {parts = [getMain 0 0 0]}

-- | can this monster walk?
canWalk :: Monster -> Bool
canWalk m = name m `notElem` 
	["Rock", "Ivy", "Tail", "Worm", "Dummy", "Golem", "Tree"]

-- | set temporary effect to a maximum from current and random values
randTemp :: Temp -> (Int, Int) -> (Monster, StdGen) -> (Monster, StdGen)
randTemp temp' bounds (mon, g) = (newMon, g') where
	(duration, g') = randomR bounds g
	newMon = setMaxTemp temp' (Just duration) mon

-- | experience for killing monster with this name
level4XP :: String -> Int
level4XP "Ivy"  = 0
level4XP "Tree" = 0
level4XP str = levelM str

-- | increase level of the monster by given xp
xpUp :: StdGen -> Monster -> Monster -> (Monster, Int, StdGen)
xpUp g mon killed = (newMon, lvls, newGen) where
	newXp = xp mon + xp killed + level4XP (name killed)
	lvls = intLog newXp - intLog (xp mon)
	(newMon, newGen) = foldr ($) (mon {xp = newXp}, g)
		$ replicate lvls levelUp

-- | upgrade monster by upgrading all its parts
levelUp :: (Monster, StdGen) -> (Monster, StdGen)
levelUp (mon, g) = (mon {parts = newParts}, g') where
	(newParts, g') = levelUpParts g $ parts mon

-- | upgrade parts by increasing 'maxhp'
levelUpParts :: StdGen -> [Part] -> ([Part], StdGen)
levelUpParts g [] = ([], g)
levelUpParts g (p:ps) = (headPart : tailParts, g'') where
	(tailParts, g') = levelUpParts g ps
	n :: Int
	(n, g'') = randomR (0, 5) g'
	headPart = p {hp = hp p + n, maxhp = maxhp p + n}

-- | generate a corpse for given monster
corpseFromMon :: Monster -> Object
corpseFromMon mon = Food {title = title', nutrition = nutr, 
	weight' = wei, rotRate = 1, rotTime = 800} where
	title' = "corpse of the " ++ name mon
	wei = 5 * sum (maxhp <$> parts mon)
	nutr = sum $ hp <$> parts mon

-- | generate partial corpse from given body part
corpseFromPart :: Monster -> Part -> Object
corpseFromPart mon part = Food {title = title', nutrition = nutr, 
	weight' = wei, rotRate = 1, 
	rotTime = if kind part == mAIN then 0 else 500} where
	title' = partToStr (kind part) ++ " of the " ++ name mon
	wei = 5 * maxhp part
	nutr = maxhp part `div` 2

-- | list of temps where all but 'Nutrition' are Nothing
startTemps :: Int -> [Maybe Int]
startTemps n = Just n : tail (const Nothing <$> (getAll :: [Temp]))

-- | intLog k === max n such that 2^n <= k
intLog :: Int -> Int
intLog = floor . flip (/) (log 2) . log . (fromIntegral :: Int -> Float)

