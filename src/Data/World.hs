module Data.World where

import Data.Define
import Data.Const
import Data.Monster

import qualified Data.Map as M
import qualified Data.Array as A
import Data.Functor ((<$>))

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

isEmptyOrPlayer :: World -> Int -> Int -> Bool
isEmptyOrPlayer world x y = x >= 0 && y >= 0 && x <= maxX && y <= maxY
	&& (case ai <$> M.lookup (x, y) (units world) of
		Nothing -> True
		Just You -> True
		_ -> False)

isValid :: World -> Int -> Int -> Int -> Int -> Bool
isValid world x y dx dy = case rez of
	Nothing -> False
	Just (x', y') -> isEmpty world x' y'
	where
		rez = dirs world (x, y, dx, dy)

isValidOrPlayer :: World -> Int -> Int -> Int -> Int -> Bool
isValidOrPlayer world x y dx dy = case rez of
	Nothing -> False
	Just (x', y') -> isEmptyOrPlayer world x' y'
	where
		rez = dirs world (x, y, dx, dy)

isSafe, isVerySafe :: World -> Int -> Int -> Int -> Int -> Bool
isSafe = isSafeByBounds (-2) 1
isVerySafe = isSafeByBounds (-1) 1

isSafeByBounds :: Int -> Int -> World -> Int -> Int -> Int -> Int -> Bool
isSafeByBounds mindh maxdh world x y dx dy = 
	not (x < 0 || y < 0 || x > maxX || y > maxY) &&
	(case rez of
	Nothing -> False
	Just (x', y') -> let
		dh = height (worldmap world A.! (x', y')) - 
			height (worldmap world A.! (x, y)) in
		dh <= maxdh && dh >= mindh)
	where
		rez = dirs world (x, y, dx, dy)

class Boolean a where
	infixr 3 &&&
	infixr 3 |||
	(&&&) :: a -> a -> a
	(|||) :: a -> a -> a

instance Boolean Bool where
	(&&&) = (&&)
	(|||) = (||)

instance Boolean b => Boolean (a -> b) where
	(f &&& g) x = f x &&& g x
	(f ||| g) x = f x ||| g x

isValidAndSafe :: World -> Int -> Int -> Int -> Int -> Bool
isValidAndSafe = isValid &&& isSafe

isPlayerNow :: World -> Bool
isPlayerNow world = isPlayer $ getFirst world
