module Data where

import Data.Set (Set(..))
import System.Random (StdGen(..))
import qualified Data.Map as M

lol = undefined

maxX = 30 :: Int
maxY = 25 :: Int

alphabet = ['a'..'z'] ++ ['A'..'Z']
notAlphabet = ['{'..]

doNothing :: IO ()
doNothing = return ()

type AIfunc = World -> Int -> Int -> World
type Inv = M.Map Char (Object, Int)
type InvGen = Float -> Inv
type StdDmg = World -> (Maybe Int, StdGen)
type MonsterGen = Int -> Int -> StdGen -> (Monster, StdGen)
type Unit = (Int, Int, Monster)

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
	inv :: M.Map Char (Object, Int),
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

data World = World {
	units :: [Unit],
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
