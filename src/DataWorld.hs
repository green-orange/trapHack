module DataWorld where

import DataDef
import Data
import DataMonster

import qualified Data.Map as M

type AIfunc = Int -> Int -> World -> World

xFirst :: World -> Int
xFirst = xF . units'

yFirst :: World -> Int
yFirst = yF . units'

getFirst :: World -> Monster
getFirst = getFirst' . units'

isEmpty :: World -> Int -> Int -> Bool
isEmpty world x y = x >= 0 && y >= 0 && x <= maxX && y <= maxY &&
	M.notMember (x, y) (units world)

isValid :: World -> Int -> Int -> Int -> Int -> Bool
isValid world x y dx dy = 
	case rez of
	Nothing -> False
	Just (x', y') -> isEmpty world x' y'
	where rez = dirs world (x, y, dx, dy)

isPlayerNow :: World -> Bool
isPlayerNow world = isPlayer $ getFirst world
