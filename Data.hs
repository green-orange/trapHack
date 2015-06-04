module Data where

import Data.Set (Set(..))
import System.Random (StdGen(..))

lol = undefined

maxX = 20 :: Int
maxY = 20 :: Int

alphabet = ['a'..'z'] ++ ['A'..'Z']
notAlphabet = ['{'..]

doNothing :: IO ()
doNothing = return ()

type AIfunc = World -> Int -> Int -> World
type InvGen = Float -> [(Char, Object, Int)]
type StdDmg = World -> (Maybe Int, StdGen)
type MonsterGen = Int -> Int -> StdGen -> (Monster, StdGen)
type Unit = (Int, Int, Monster)

data Part = Part {
	hp :: Int,
	maxhp :: Int,
	kind :: Int,
	idP :: Int,
	regVel :: Int,
	aliveP :: Bool
}
data AI = You | AI AIfunc
data Monster = Monster {
	ai :: AI,
	parts :: [Part],
	x :: Int,
	y :: Int,
	name :: String,
	stddmg :: StdDmg,
	inv :: [(Char, Object, Int)],
	slowness :: Int,
	time :: Int
}

type Terrain = Int
data Object =
	Potion {
		title :: String,
		act :: Monster -> Monster
	} | 
	Wand {
		title :: String,
		act :: Monster -> Monster,
		range :: Int,
		charge :: Int
	} | 
	Trap {
		title :: String,
		num :: Int
	}

instance Eq Object where
	(Potion t _) == (Potion t' _) = t == t'
	(Trap t _) == (Trap t' _) = t == t'
	_ == _ = False

data World = World {
	units :: [Unit],
	message :: String,
	items :: [(Int, Int, Object, Int)],
	action :: Char,
	stdgen :: StdGen,
	wave :: Int,
	toPick :: Set Char,
	store :: [Char],
	worldmap :: [[Terrain]],
	dirs :: ((Int, Int, Int, Int)) -> Maybe (Int, Int)
}
