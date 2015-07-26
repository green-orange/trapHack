module Data.World where

import Data.Define
import Data.Const
import Data.Monster

import qualified Data.Map as M
import qualified Data.Array as A
import Data.Functor ((<$>))

-- | a function to realize AI
-- usage of (f :: AIfunc): f x y peace world where
-- x, y are player coords, peace is False when you want to attack player
-- and False otherwise
type AIfunc = Int -> Int -> Bool -> World -> World

-- | interface to get x coordinate of the current monster from the world
xFirst :: World -> Int
xFirst = xF . units'
-- | interface to get y coordinate of the current monster from the world
yFirst :: World -> Int
yFirst = yF . units'
-- | interface to get the current monster from the world
getFirst :: World -> Monster
getFirst = getFirst' . units'

-- | is cell with given coords exist?
isCell :: Int -> Int -> Bool
isCell x y = x >= 0 && y >= 0 && x <= maxX && y <= maxY
-- | is cell with given coords empty (doesn't consist any monsters)?
isEmpty :: World -> Int -> Int -> Bool
isEmpty world x y = isCell x y && M.notMember (x, y) (units world)
-- | is cell with given coords empty or contsins player?
isEmptyOrPlayer :: World -> Int -> Int -> Bool
isEmptyOrPlayer world x y = isCell x y 
	&& terrain (worldmap world A.! (x, y)) == Empty
	&& (case ai <$> M.lookup (x, y) (units world) of
		Nothing -> True
		Just You -> True
		_ -> False)
-- | is step from (x, y) cell to (dx, dy) direction valid (without attack)?
isValid :: World -> Int -> Int -> Int -> Int -> Bool
isValid world x y dx dy = 
	isCell x y && isEmpty world (x + dx) (y + dy)
-- | is step from (x, y) cell to (dx, dy) direction valid
-- (without attack or with player attack)?
isValidOrPlayer :: World -> Int -> Int -> Int -> Int -> Bool
isValidOrPlayer world x y dx dy = 
	isCell x y && isEmptyOrPlayer world (x + dx) (y + dy)

isSafe, isVerySafe :: World -> Int -> Int -> Int -> Int -> Bool
-- | can i do this step with not so big wounds?
isSafe = isSafeByBounds (-2) 1
-- | can i do this step without any wounds?
isVerySafe = isSafeByBounds (-1) 1

-- | is different of heights before this step and after it
-- at most 'maxdh' and at least 'mindh'?
isSafeByBounds :: Int -> Int -> World -> Int -> Int -> Int -> Int -> Bool
isSafeByBounds mindh maxdh world x y dx dy = 
	isCell x y && isCell (x + dx) (y + dy) && ( let
		dh = height (worldmap world A.! (x + dx, y + dy)) - 
			height (worldmap world A.! (x, y)) in
		dh <= maxdh && dh >= mindh)

-- | class to use AND and OR with functions from some arguments to Bool
class Boolean a where
	infixr 3 &&&
	infixr 3 |||
	(&&&) :: a -> a -> a
	(|||) :: a -> a -> a
-- | obvious realization for Bool
instance Boolean Bool where
	(&&&) = (&&)
	(|||) = (||)
-- | realization for functions
instance Boolean b => Boolean (a -> b) where
	(f &&& g) x = f x &&& g x
	(f ||| g) x = f x ||| g x
-- | is this step valid and safe simultaneously
isValidAndSafe :: World -> Int -> Int -> Int -> Int -> Bool
isValidAndSafe = isValid &&& isSafe
-- | is current monster a player? 
isPlayerNow :: World -> Bool
isPlayerNow world = isPlayer $ getFirst world
