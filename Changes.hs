module Changes where

import Data
import Parts

import qualified Data.Set as S
import UI.HSCurses.Curses (Key (..))
import System.Random (StdGen)
import qualified Data.Map as M

{- Part -}

changeHP :: Int -> Part -> Part
changeHP n part = part {hp = n}

{- Units -}

update :: Int -> Int -> Units -> Units
update x' y' uns = 
	if x' == xF uns && y' == yF uns
	then uns {getFirst' = list uns M.! (x', y')}
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

delObj :: Key -> Monster -> Monster
delObj c m = changeInv newInv m where
	newInv = M.update maybeUpd (fromKey c) $ inv m
	maybeUpd (_, 1) = Nothing
	maybeUpd (o, n) = Just (o, n - 1)

delAllObj :: Key -> Monster -> Monster
delAllObj c m = changeInv newInv m where
	newInv = M.delete (fromKey c) $ inv m
		
decChargeByKey :: Char -> Monster -> Monster
decChargeByKey c m = changeInv newInv m where
	newInv = M.adjust (\(o, n) -> (decCharge o, n)) c $ inv m

changePoison :: Maybe Int -> Monster -> Monster
changePoison n m = m {poison = n}

setMaxPoison :: Maybe Int -> Monster -> Monster
setMaxPoison n m = m {poison = max n $ poison m}

{- World -}

changeAction :: Char -> World -> World
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
	mon = units w M.! (xFirst w, yFirst w)

addMessages :: [(String, Int)] -> World -> World
addMessages s w = w {message = message w ++ s}
	
addMessage :: (String, Int) -> World -> World
addMessage ("", _) = id
addMessage s = addMessages [s]

clearMessage :: World -> World
clearMessage w = w {message = []}

changeGen :: StdGen -> World -> World
changeGen g w = w {stdgen = g}

changeChars :: S.Set Char -> World -> World
changeChars cs w = w {chars = cs}

changeChar :: Key -> World -> World
changeChar c w = w {chars = newPick} where
	KeyChar sym = c
	newPick =
		if S.member sym $ chars w
		then S.delete sym $ chars w
		else S.insert sym $ chars w
		
addItem :: (Int, Int, Object, Int) -> World -> World
addItem i w = w {items = items'} where
	items' = addItem' i $ items w

changeMap :: Int -> Int -> Terrain -> World -> World
changeMap x y t w = w {worldmap = worldmap'} where
	worldmap' = changeElem2 x y t $ worldmap w

spawnMon :: MonsterGen -> Int -> Int -> World -> World
spawnMon mgen x y w = changeMons (changeList 
	(M.insert (x, y) (newMon {time = (time $ getFirst w) + effectiveSlowness newMon})
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
		then mon {time = time mon + (effectiveSlowness mon) * 3 `div` 2}
		else mon
	newMons = mapU ch $ units' w

changeShiftOn, changeSlotOn :: Int -> World -> World
changeShiftOn n w = w {shift = mod (shift w + n) $ length $ parts $ getFirst w}
changeSlotOn n w = w {slot = toEnum $ flip mod sLOTS $ (+n) $ fromEnum $ slot w}

downshift, upshift, decslot, incslot :: World -> World
downshift = changeShiftOn 1
upshift = changeShiftOn (-1)
incslot = changeSlotOn 1
decslot = changeSlotOn (-1)

{- Object -}

decCharge :: Object -> Object
decCharge obj = obj {charge = charge obj - 1}

enchant :: Int -> Object -> Object
enchant n obj = obj {enchantment = enchantment obj + n} 

{- Other -}

changeElem :: Int -> a -> [a] -> [a]
changeElem x t ts
	| x == 0 = t : tail ts
	| x > 0 = head ts : changeElem (x - 1) t (tail ts)
	| otherwise = error "negative index in changeElem function"

changeElem2 :: Int -> Int -> a -> [[a]] -> [[a]]
changeElem2 x y t tss
	| x > 0 = head tss : changeElem2 (x - 1) y t (tail tss)
	| x == 0 = changeElem y t (head tss) : tail tss
	| otherwise = error "negative index in changeElem2 function"

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
		this = filter (\(x', y', obj', _) -> x == x' && y == y' && obj == obj') $ list'

fromKey :: Key -> Char
fromKey (KeyChar c) = c
fromKey _ = ' '
