module Changes where

import Data
import Utils4all

import Data.Set (member, delete, insert)
import UI.HSCurses.Curses (Key (..))
import System.Random (StdGen)

{- Part -}

changeHP :: Int -> Part -> Part
changeHP n part = part {hp = n}

{- Monster -}

changeParts :: [Part] -> Monster -> Monster
changeParts ps mon = mon {parts = ps}

changeTime :: Int -> Monster -> Monster
changeTime t mon = mon {time = t}
	
tickDownMon :: Monster -> Monster
tickDownMon m = changeTime (time m - 1) m

resetTimeMon :: Monster -> Monster
resetTimeMon m = changeTime (effectiveSlowness m) m

changeInv :: [Inv] -> Monster -> Monster
changeInv inv mon = mon {inv = inv}

delObj :: Key -> Monster -> Monster
delObj c m = changeInv newInv m where
	[obj] = filter (\(x,_,_) -> KeyChar x == c) $ inv m
	newInv = 
		if third obj == 1
		then filter (\(x, _, _) -> KeyChar x /= c) $ inv m
		else map decCount $ inv m
	decCount (x, o, n) = 
		if KeyChar x == c
		then (x, o, n - 1)
		else (x, o, n)

decChargeByKey :: Char -> Monster -> Monster
decChargeByKey c m = changeInv newInv m where
	newInv = map (\(x, obj,n) -> if x == c then (x, decCharge obj, n) else (x, obj, n)) $ inv m

changeWeapon :: Key -> Monster -> Monster
changeWeapon c mon = mon {weapon = fromKey c}

{- World -}

changeAction :: Char -> World -> World
changeAction c w = w {action = c}

changeStore :: [Char] -> World -> World
changeStore c w = w {store = c}

changeMon :: Monster -> World -> World
changeMon mon w = changeMons ((x, y, mon) : (tail $ units w)) w
	where (x, y, _) = head $ units w

changeMons :: [Unit] -> World -> World
changeMons mons w = w {units = mons}

addMessages :: [(String, Int)] -> World -> World
addMessages s w = w {message = message w ++ s}
	
addMessage :: (String, Int) -> World -> World
addMessage ("", _) = id
addMessage s = addMessages [s]

clearMessage :: World -> World
clearMessage w = w {message = []}

changeGen :: StdGen -> World -> World
changeGen g w = w {stdgen = g}

changePickFirst :: Key -> World -> World
changePickFirst c w = w {toPick = newPick} where
	KeyChar sym = c
	newPick =
		if member sym $ toPick w
		then delete sym $ toPick w
		else insert sym $ toPick w
		
addItem :: (Int, Int, Object, Int) -> World -> World
addItem i w = w {items = items'} where
	items' = addItem' i $ items w

changeMap :: Int -> Int -> Terrain -> World -> World
changeMap x y t w = w {worldmap = worldmap'} where
	worldmap' = changeElem2 x y t $ worldmap w
	
maybeAddMessage :: String -> World -> World
maybeAddMessage msg w = 
	if isPlayerNow w
	then addMessage (msg, yELLOW) w
	else w
	
addNeutralMessage :: String -> World -> World
addNeutralMessage msg w = 
	if isPlayerNow w
	then addMessage (msg, gREEN) w
	else addMessage (msg, yELLOW) w
	
addDefaultMessage :: String -> World -> World
addDefaultMessage msg w = addMessage (msg, dEFAULT) w

spawnMon :: MonsterGen -> Int -> Int -> World -> World
spawnMon mgen x y w = changeMons (units w ++ [(x, y, newMon)]) $ changeGen g w where
	(newMon, g) = mgen x y $ stdgen w

{- Object -}

decCharge :: Object -> Object
decCharge obj = obj {charge = charge obj - 1}

{- Other -}

changeElem :: Int -> a -> [a] -> [a]
changeElem x t ts
	| x == 0 = t : tail ts
	| x > 0 = head ts : changeElem (x - 1) t (tail ts)

changeElem2 :: Int -> Int -> a -> [[a]] -> [[a]]
changeElem2 x y t tss
	| x > 0 = head tss : changeElem2 (x - 1) y t (tail tss)
	| x == 0 = changeElem y t (head tss) : tail tss

addItem' :: (Int, Int, Object, Int) -> [(Int, Int, Object, Int)] -> [(Int, Int, Object, Int)]
addItem' i@(x, y, obj, n) list = 
	if null this
	then i : list
	else map change list
	where
		change i'@(x', y', obj', n') = 
			if x == x' && y == y' && obj == obj'
			then (x', y', obj', n + n')
			else i'
		this = filter (\(x', y', obj', _) -> x == x' && y == y' && obj == obj') $ list
