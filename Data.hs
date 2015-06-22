module Data where

import Data.Set (Set)
import System.Random (StdGen)
import qualified Data.Map as M

lol :: a
lol = undefined

maxX, maxY :: Int
maxX = 39
maxY = 19

eMPTY, bEARTRAP, fIRETRAP, pOISONTRAP, tRAPSNUM, mAGICTRAP :: Int
eMPTY      = 0
bEARTRAP   = 1
fIRETRAP   = 2
pOISONTRAP = 3
mAGICTRAP  = 4
tRAPSNUM = mAGICTRAP

dEFAULT, gREEN, yELLOW, rED, rEDiNVERSE, cYAN, mAGENTA, bLUE :: Int
dEFAULT	   = tRAPSNUM + 1
gREEN	   = tRAPSNUM + 2
yELLOW	   = tRAPSNUM + 3
rED 	   = tRAPSNUM + 4
rEDiNVERSE = tRAPSNUM + 5
cYAN	   = tRAPSNUM + 6
mAGENTA	   = tRAPSNUM + 7
bLUE       = tRAPSNUM + 8

sLOTS :: Int
sLOTS = fromEnum (maxBound :: Slot) - fromEnum (minBound :: Slot) + 1
data Slot = WeaponSlot | ArmorSlot | JewelrySlot deriving (Enum, Bounded, Eq)

data Elem = Fire | Poison deriving (Enum, Show, Bounded)

alphabet, notAlphabet :: String
alphabet = ['a'..'z'] ++ ['A'..'Z']
notAlphabet = ['{'..]

doNothing :: IO ()
doNothing = return ()

type AIfunc = Int -> Int -> World -> World
type Inv = M.Map Char (Object, Int)
type InvGen = Float -> Inv
type StdDmg = World -> (Maybe Int, StdGen)
type MonsterGen = StdGen -> (Monster, StdGen)

data Units = Units {
	xF :: Int,
	yF :: Int,
	getFirst' :: Monster,
	list :: M.Map (Int, Int) Monster
}

data Part = Part {
	hp :: Int,
	maxhp :: Int,
	kind :: Int,
	idP :: Int,
	regVel :: Int,
	objectKeys :: [Char]
}
data AI = You | AI AIfunc
data Monster = Monster {
	ai :: AI,
	parts :: [Part],
	name :: String,
	stddmg :: StdDmg,
	inv :: Inv,
	slowness :: Int,
	time :: Int,
	poison :: Maybe Int,
	res :: [Int]
}

type Terrain = Int
data Object =
	Something |
	Potion {
		title :: String,
		act :: (Monster, StdGen) -> (Monster, StdGen)
	} | 
	Wand {
		title :: String,
		act :: (Monster, StdGen) -> (Monster, StdGen),
		range :: Int,
		charge :: Int
	} |
	Scroll {
		title :: String,
		actw :: World -> World
	} | 
	Trap {
		title :: String,
		num :: Int
	} |
	Missile {
		title :: String,
		objdmg' :: StdDmg,
		launcher :: String,
		enchantment :: Int
	} |
	Launcher {
		title :: String,
		count' :: Int,
		category :: String,
		enchantment :: Int
	} |
	Weapon {
		title :: String,
		objdmg' :: StdDmg,
		enchantment :: Int
	} |
	Armor {
		title :: String,
		ac' :: Int,
		bind :: Int,
		enchantment :: Int
	} |
	Jewelry {
		title :: String,
		enchantment :: Int,
		bind :: Int,
		effectOn :: Int -> Monster -> Monster,
		effectOff :: Int -> Monster -> Monster
	}
	
objdmg :: Object -> StdDmg
objdmg obj w = (fmap (+ enchantment obj) n, g) where
	(n, g) = objdmg' obj w

count :: Object -> Int
count obj = count' obj + enchantment obj

ac :: Object -> Int
ac obj = ac' obj + enchantment obj

instance Eq Object where
	(Potion t _) == (Potion t' _) = t == t'
	(Trap t _) == (Trap t' _) = t == t'
	(Missile t _ _ _) == (Missile t' _ _ _) = t == t'
	(Scroll t _) == (Scroll t' _) = t == t'
	_ == _ = False

isStackable :: Object -> Bool
isStackable obj = obj == obj

data World = World {
	units' :: Units,
	message :: [(String, Int)],
	items :: [(Int, Int, Object, Int)],
	action :: Char,
	stdgen :: StdGen,
	wave :: Int,
	chars :: Set Char,
	worldmap :: [[Terrain]],
	dirs :: (Int, Int, Int, Int) -> Maybe (Int, Int),
	stepsBeforeWave :: Int,
	prevAction :: Char,
	shift :: Int,
	slot :: Slot
}

xFirst :: World -> Int
xFirst = xF . units'

yFirst :: World -> Int
yFirst = yF . units'

getFirst :: World -> Monster
getFirst = getFirst' . units'

units :: World -> M.Map (Int, Int) Monster
units = list . units'

mapU :: ((Int, Int) -> Monster -> Monster) -> Units -> Units
mapU f uns = uns 
	{list = M.mapWithKey f $ list uns,
	getFirst' = f (xF uns, yF uns) $ getFirst' uns}
	
insertU :: (Int, Int) -> Monster -> Units -> Units
insertU k m uns = uns {list = M.insert k m $ list uns}

deleteU :: (Int, Int) -> Units -> Units
deleteU k uns = uns {list = M.delete k $ list uns}

isEmpty :: World -> Int -> Int -> Bool
isEmpty world x y = x >= 0 && y >= 0 && x <= maxX && y <= maxY &&
	(M.notMember (x, y) $ units world)

isValid :: World -> Int -> Int -> Int -> Int -> Bool
isValid world x y dx dy = 
	case rez of
	Nothing -> False
	Just (x', y') -> isEmpty world x' y'
	where rez = dirs world (x, y, dx, dy)

isPlayer :: Monster -> Bool
isPlayer mon = case ai mon of
	You -> True
	AI _ -> False

isPlayerNow :: World -> Bool
isPlayerNow world = isPlayer $ getFirst world
