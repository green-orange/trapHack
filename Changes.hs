module Changes where

import Data
import Utils4all

import Data.Set (member, delete, insert)
import UI.HSCurses.Curses (Key (..))
import System.Random (StdGen)

{- Part -}

changeHP :: Int -> Part -> Part
changeHP n (Part _ maxhp kind idP regVel aliveP) = 
		    Part n maxhp kind idP regVel aliveP

{- Monster -}

changeParts :: [Part] -> Monster -> Monster
changeParts ps (Monster ai _  name stddmg inv slowness time weapon) =
				Monster ai ps name stddmg inv slowness time weapon

changeTime :: Int -> Monster -> Monster
changeTime t (Monster ai parts name stddmg inv slowness _ weapon) =
			  Monster ai parts name stddmg inv slowness t weapon
	
tickDownMon :: Monster -> Monster
tickDownMon m = changeTime (time m - 1) m

resetTimeMon :: Monster -> Monster
resetTimeMon m = changeTime (effectiveSlowness m) m

changeInv :: [Inv] -> Monster -> Monster
changeInv inv (Monster ai parts name stddmg _   slowness time weapon) =
			   Monster ai parts name stddmg inv slowness time weapon

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
changeWeapon c (Monster ai parts name stddmg inv slowness time _) =
				Monster ai parts name stddmg inv slowness time weapon where
	weapon = fromKey c

{- World -}

changeAction :: Char -> World -> World
changeAction c (World units message items _ stdgen wave toPick store worldmap dirs) =
				World units message items c stdgen wave toPick store worldmap dirs

changeStore :: [Char] -> World -> World
changeStore c (World units message items action stdgen wave toPick _ worldmap dirs) =
			   World units message items action stdgen wave toPick c worldmap dirs

changeMon :: Monster -> World -> World
changeMon mon w = changeMons ((x, y, mon) : (tail $ units w)) w
	where (x, y, _) = head $ units w

changeMons :: [Unit] -> World -> World
changeMons mons (World _    message items action stdgen wave toPick store worldmap dirs) =
			     World mons message items action stdgen wave toPick store worldmap dirs

addMessages :: [(String, Int)] -> World -> World
addMessages s w@(World units old  items action stdgen wave toPick store worldmap dirs) =
				World units s'   items action stdgen wave toPick store worldmap dirs
	where s' = old ++ s
	
addMessage :: (String, Int) -> World -> World
addMessage ("", _) = id
addMessage s = addMessages [s]

clearMessage :: World -> World
clearMessage (World units _  items action stdgen wave toPick store worldmap dirs) =
			  World units [] items action stdgen wave toPick store worldmap dirs

changeGen :: StdGen -> World -> World
changeGen g (World units message items action _ wave toPick store worldmap dirs) =
			 World units message items action g wave toPick store worldmap dirs

changePickFirst :: Key -> World -> World
changePickFirst c (World units message items action stdgen wave oldPick store worldmap dirs) =
				   World units message items action stdgen wave toPick  store worldmap dirs where
	KeyChar sym = c
	toPick =
		if member sym oldPick
		then delete sym oldPick
		else insert sym oldPick
		
addItem :: (Int, Int, Object, Int) -> World -> World
addItem i (World units message olditems action stdgen wave toPick store worldmap dirs) =
		   World units message items    action stdgen wave toPick store worldmap dirs where
	items = addItem' i olditems

changeMap :: Int -> Int -> Terrain -> World -> World
changeMap x y t (World units message items action stdgen wave toPick store oldmap	dirs) =
				 World units message items action stdgen wave toPick store worldmap dirs where
	worldmap = changeElem2 x y t oldmap
	
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
decCharge obj = Wand {
	title = title obj,
	act = act obj,
	range = range obj,
	charge = charge obj - 1
}

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
