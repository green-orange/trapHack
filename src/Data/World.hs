module Data.World where

import Data.Define
import Data.Const
import Data.Monster

import qualified Data.Map as M
import qualified Data.Array as A

type AIfunc = Int -> Int -> Bool -> World -> World

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
isValid world x y dx dy = case rez of
	Nothing -> False
	Just (x', y') -> isEmpty world x' y'
	where
		rez = dirs world (x, y, dx, dy)

isSafe :: World -> Int -> Int -> Int -> Int -> Bool
isSafe world x y dx dy = case rez of
	Nothing -> False
	Just (x', y') -> let
		dh = height (worldmap world A.! (x', y')) - 
			height (worldmap world A.! (x, y)) in
		dh <= 1 && dh >= -2
	where
		rez = dirs world (x, y, dx, dy)

isValidAndSafe :: World -> Int -> Int -> Int -> Int -> Bool
isValidAndSafe world x y dx dy = isValid world x y dx dy
	&& isSafe world x y dx dy

isPlayerNow :: World -> Bool
isPlayerNow world = isPlayer $ getFirst world
