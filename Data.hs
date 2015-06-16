module Data where

import Data.Set (Set(..))
import System.Random (StdGen(..))
import qualified Data.Map as M

lol = undefined

maxX = 30 :: Int
maxY = 25 :: Int

eMPTY    = 0 :: Int
bEARTRAP = 1 :: Int
fIRETRAP = 2 :: Int
tRAPSNUM = fIRETRAP

dEFAULT	   = tRAPSNUM + 1
gREEN	   = tRAPSNUM + 2
yELLOW	   = tRAPSNUM + 3
rED 	   = tRAPSNUM + 4
rEDiNVERSE = tRAPSNUM + 5
cYAN	   = tRAPSNUM + 6
mAGENTA	   = tRAPSNUM + 7
bLUE       = tRAPSNUM + 8

alphabet = ['a'..'z'] ++ ['A'..'Z']
notAlphabet = ['{'..]

doNothing :: IO ()
doNothing = return ()

type AIfunc = World -> Int -> Int -> World
type Inv = M.Map Char (Object, Int)
type InvGen = Float -> Inv
type StdDmg = World -> (Maybe Int, StdGen)
type MonsterGen = StdGen -> (Monster, StdGen)

data Units = Units {
	x :: Int,
	y :: Int,
	getFirst' :: Monster,
	list :: M.Map (Int, Int) Monster
}

data Part = Part {
	hp :: Int,
	maxhp :: Int,
	kind :: Int,
	idP :: Int,
	regVel :: Int
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
	weapon :: Char
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
		objdmg :: StdDmg,
		launcher :: String
	} |
	Launcher {
		title :: String,
		count :: Int,
		category :: String
	} |
	Weapon {
		title :: String,
		objdmg :: StdDmg
	}

instance Eq Object where
	(Potion t _) == (Potion t' _) = t == t'
	(Trap t _) == (Trap t' _) = t == t'
	(Missile t _ _) == (Missile t' _ _) = t == t'
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
	toPick :: Set Char,
	worldmap :: [[Terrain]],
	dirs :: (Int, Int, Int, Int) -> Maybe (Int, Int),
	stepsBeforeWave :: Int,
	prevAction :: Char
}

xFirst :: World -> Int
xFirst = x . units'

yFirst :: World -> Int
yFirst = y . units'

getFirst :: World -> Monster
getFirst = getFirst' . units'

units :: World -> M.Map (Int, Int) Monster
units = list . units'

mapU :: ((Int, Int) -> Monster -> Monster) -> Units -> Units
mapU f uns = uns 
	{list = M.mapWithKey f $ list uns,
	getFirst' = f (x uns, y uns) $ getFirst' uns}
	
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

isPlayerNow :: World -> Bool
isPlayerNow world = (name $ getFirst world) == "You"
