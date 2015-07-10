module Init where

import Data
import Parts
import Colors
import Texts
import DataDef
--import Stuff

import System.Random (StdGen)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Array

rectdirs :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Maybe (Int, Int)
rectdirs (xmin, ymin, xmax, ymax) (x, y, dx, dy) =
	if xnew >= xmin && xnew <= xmax && ynew >= ymin && ynew <= ymax
	then Just (xnew, ynew)
	else Nothing
	where
		xnew = x + dx
		ynew = y + dy
		
initUnits :: Units
initUnits = Units {
	xF = x',
	yF = y',
	getFirst' = getPlayer,
	list = M.singleton (x', y') getPlayer
} where
	x' = div maxX 2
	y' = div maxY 2

initWorld :: String -> StdGen -> World
initWorld username gen = World {
	worldmap = listArray ((0,0), (maxX,maxY)) $ repeat Empty,
	dirs = rectdirs (0, 0, maxX, maxY),
	units' = initUnits,
	message = [(msgWelcome username, bLUE)],
	items = [],
	action = ' ',
	stdgen = gen,
	wave = 1,
	chars = S.empty,
	prevAction = ' ',
	stepsBeforeWave = 1,
	shift = 0,
	slot = toEnum 0,
	xInfo = 0,
	yInfo = 0
}

getPlayer :: Monster
getPlayer = Monster {
	ai = You,
	parts = zipWith ($) 
		[getBody 1 40, 
		 getHead 1 30, 
		 getLeg  2 20, 
		 getLeg  2 20, 
		 getArm  2 20, 
		 getArm  2 20]
		 [0..],
	name = "You",
	stddmg = ((1,10), 0.2),
	inv = M.empty,
	slowness = 100,
	time = 100,
	res = map (const 0) (getAll :: [Elem]),
	intr = map (const 0) (getAll :: [Intr]),
	temp = map (const Nothing) (getAll :: [Temp]),
	idM = 0,
	xp = 1
}
