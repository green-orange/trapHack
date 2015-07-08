module Data where

import Data.Set (Set)
import System.Random (StdGen)
import qualified Data.Map as M
import Data.Array

lol :: a
lol = undefined

maxX, maxY, xSight, ySight :: Int
maxX = 99
maxY = 59
xSight = 20
ySight = 10

sLOTS :: Int
sLOTS = fromEnum (maxBound :: Slot) - fromEnum (minBound :: Slot) + 1
data Slot = WeaponSlot | ArmorSlot | JewelrySlot deriving (Enum, Bounded, Eq)

data Elem = Fire | Poison' | Cold deriving (Enum, Bounded)

instance Show Elem where
	show Fire = "Fire"
	show Poison' = "Poison"
	show Cold = "Cold"
	
data Intr = Teleport deriving (Enum, Show, Bounded)

data Temp = Poison | Stun | Conf deriving (Enum, Show, Bounded)

data Terrain = Empty | BearTrap | FireTrap | PoisonTrap | MagicTrap 
	deriving (Eq)

instance Show Terrain where
	show Empty = "empty cell"
	show BearTrap = "bear trap"
	show FireTrap = "fire trap"
	show PoisonTrap = "poison trap"
	show MagicTrap = "magic trap"

getAll :: (Bounded a, Enum a) => [a]
getAll = [minBound..maxBound]

alphabet, notAlphabet :: String
alphabet = ['a'..'z'] ++ ['A'..'Z']
notAlphabet = ['{'..]

doNothing :: IO ()
doNothing = return ()

shiftDown :: Int
shiftDown = 5

type AIfunc = Int -> Int -> World -> World
type Inv = M.Map Char (Object, Int)
type InvGen = StdGen -> (Inv, StdGen)
type StdDmg = World -> (Maybe Int, StdGen)
type MonsterGen = StdGen -> (Monster, StdGen)

emptyInv :: InvGen
emptyInv g = (M.empty, g)

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
	objectKeys :: String
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
	res :: [Int],
	intr :: [Int],
	temp :: [Maybe Int],
	idM :: Int
}

data Object =
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
		num :: Terrain
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
		enchantment :: Int,
		weight' :: Int
	} |
	Weapon {
		title :: String,
		objdmg' :: StdDmg,
		enchantment :: Int,
		weight' :: Int
	} |
	Armor {
		title :: String,
		ac' :: Int,
		bind :: Int,
		enchantment :: Int,
		weight' :: Int
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

weight :: Object -> Int
weight (Potion {})  = 20
weight (Wand {})    = 7
weight (Scroll {})  = 5
weight (Trap {})    = 50
weight (Missile {}) = 2
weight (Jewelry {}) = 10
weight obj = weight' obj

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
	worldmap :: Array (Int, Int) Terrain,
	dirs :: (Int, Int, Int, Int) -> Maybe (Int, Int),
	stepsBeforeWave :: Int,
	prevAction :: Char,
	shift :: Int,
	slot :: Slot,
	xInfo :: Int,
	yInfo :: Int
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
	M.notMember (x, y) (units world)

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
