module Utils.Changes where

import Data.Const
import Data.World
import Data.Monster
import Data.Define
import Monsters.Parts
import IO.Texts

import qualified Data.Set as S
import System.Random (StdGen)
import qualified Data.Map as M
import Data.Array
import Control.Arrow (first)

{- Part -}

changeHP :: Int -> Part -> Part
changeHP n part = part {hp = n}

{- Units -}

update :: Int -> Int -> Units -> Units
update x' y' uns = 
	if x' == xF uns && y' == yF uns
	then case M.lookup (x', y') $ list uns of
		Nothing -> error $ msgWE "update" 
		Just mon -> uns {getFirst' = mon}
	else uns

changeList :: M.Map (Int, Int) Monster -> Units -> Units
changeList m uns = uns {list = m}

{- Monster -}

changeParts :: [Part] -> Monster -> Monster
changeParts ps mon = mon {parts = ps}

changeTime :: Int -> Monster -> Monster
changeTime t mon = mon {time = t}

tickFirstMon :: Monster -> Monster
tickFirstMon m = changeTime (effectiveSlowness m + time m) m

changeInv :: Inv -> Monster -> Monster
changeInv inv' mon = mon {inv = inv'}

delObj :: Char -> Monster -> Monster
delObj c m = changeInv newInv m where
	newInv = M.update maybeUpd c $ inv m
	maybeUpd (_, 1) = Nothing
	maybeUpd (o, n) = Just (o, n - 1)

delAllObj :: Char -> Monster -> Monster
delAllObj c m = changeInv newInv m where
	newInv = M.delete c $ inv m
		
decChargeByKey :: Char -> Monster -> Monster
decChargeByKey c m = changeInv newInv m where
	newInv = M.adjust (first decCharge) c $ inv m

addRes :: Elem -> Int -> Monster -> Monster
addRes elem' n m = m {res = changeElem pos new $ res m} where
	pos = fromEnum elem'
	new = res m !! pos + n

addIntr :: Intr -> Int -> Monster -> Monster
addIntr intr' n m = m {intr = changeElem pos new $ intr m} where
	pos = fromEnum intr'
	new = intr m !! pos + n

changeTemp:: Temp -> Maybe Int -> Monster -> Monster
changeTemp temp' n m = m {temp = changeElem pos n $ temp m} where
	pos = fromEnum temp'

setMaxTemp :: Temp -> Maybe Int -> Monster -> Monster
setMaxTemp temp' n m = changeTemp temp' (max n old) m where
	pos = fromEnum temp'
	old = temp m !! pos

{- World -}

changeAction :: Action -> World -> World
changeAction c w = w {action = c}

changeMon :: Monster -> World -> World
changeMon mon w = changeMons newMons w where
	newMons = update x y $ (units' w) {list = M.insert (x, y) mon $ list $ units' w}
	x = xFirst w
	y = yFirst w

changeMons :: Units -> World -> World
changeMons mons w = w {units' = mons}

changeMoveFirst :: Int -> Int -> World -> World
changeMoveFirst x y w = changeMons newMons w where
	newMons = (units' w) {
		xF = x,
		yF = y,
		list = M.insert (x, y) mon $ M.delete (xFirst w, yFirst w) $ units w
	}
	mon = getFirst w

addMessages :: [(String, Int)] -> World -> World
addMessages s w = w {message = message w ++ s}
	
addMessage :: (String, Int) -> World -> World
addMessage ("", _) = id
addMessage s = addMessages [s]

clearMessage :: Int -> World -> World
clearMessage width w = w {message = 
	dropAccum (message w) maxLen} where
	maxLen = width * (shiftDown - 1) - 2
	dropAccum :: [([a], b)] -> Int -> [([a], b)]
	dropAccum [] _ = []
	dropAccum arg@((x, _):xs) n = 
		if length x <= n
		then dropAccum xs $ n - length x - 1
		else arg

changeGen :: StdGen -> World -> World
changeGen g w = w {stdgen = g}

changeChars :: S.Set Char -> World -> World
changeChars cs w = w {chars = cs}

changeChar :: Char -> World -> World
changeChar c w = w {chars = newPick} where
	sym = c
	newPick =
		if S.member sym $ chars w
		then S.delete sym $ chars w
		else S.insert sym $ chars w
		
addItem :: (Int, Int, Object, Int) -> World -> World
addItem i w = w {items = items'} where
	items' = addItem' i $ items w

changeMap :: Int -> Int -> Terrain -> World -> World
changeMap x y t w = w {worldmap = worldmap'} where
	worldmap' = worldmap w // [((x, y), t)]

spawnMon :: MonsterGen -> Int -> Int -> World -> World
spawnMon mgen x y w = changeMons (changeList 
	(M.insert (x, y) (newMon {time = time (getFirst w) + effectiveSlowness newMon})
		$ units w) $ units' w) $ changeGen g w where
	(newMon, g) = mgen $ stdgen w
	
paralyse :: Int -> Int -> World -> World
paralyse dx dy w = changeMons newMons w where
	xNow = xFirst w
	yNow = yFirst w
	x = xNow + dx
	y = yNow + dy
	ch (x', y') mon = 
		if x == x' && y == y'
		then mon {time = time mon + effectiveSlowness mon * 3 `div` 2}
		else mon
	newMons = mapU ch $ units' w

changeShiftOn, changeSlotOn :: Int -> World -> World
changeShiftOn n w = w {shift = mod (shift w + n) $ length $ parts $ getFirst w}
changeSlotOn n w = w {slot = toEnum $ flip mod sLOTS $ (+n) $ fromEnum $ slot w}

{- Object -}

decCharge :: Object -> Object
decCharge obj = obj {charge = charge obj - 1}

enchant :: Int -> Object -> Object
enchant n obj = obj {enchantment = enchantment obj + n} 

{- Other -}

changeElem :: Int -> a -> [a] -> [a]
changeElem _ _ [] = error $ msgWE "changeElem"
changeElem x t (a:as)
	| x == 0 = t : as
	| x > 0 = a : changeElem (x - 1) t as
	| otherwise = error $ msgWE "changeElem"

addItem' :: (Int, Int, Object, Int) -> [(Int, Int, Object, Int)] -> [(Int, Int, Object, Int)]
addItem' i@(x, y, obj, n) list' = 
	if null this
	then i : list'
	else map change list'
	where
		change i'@(x', y', obj', n') = 
			if x == x' && y == y' && obj == obj'
			then (x', y', obj', n + n')
			else i'
		this = filter (\(x', y', obj', _) -> x == x' && y == y' && obj == obj') list'
