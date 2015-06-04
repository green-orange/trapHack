module Changes where

import Data
import Utils4all
import Data.Set (member, delete, insert)
import UI.HSCurses.Curses (Key (..))

import System.Random (StdGen)

changeHP :: Int -> Part -> Part
changeHP n part = Part {
	hp = n,
	maxhp = maxhp part,
	kind = kind part,
	idP = idP part,
	regVel = regVel part,
	aliveP = aliveP part
}

changeAction :: Char -> World -> World
changeAction c w = World {
	units = units w,
	message = message w,
	items = items w,
	action = c,
	stdgen = stdgen w,
	wave = wave w,
	toPick = toPick w,
	store = store w,
	worldmap = worldmap w,
	dirs = dirs w
}

changeStore :: [Char] -> World -> World
changeStore c w = World {
	units = units w,
	message = message w,
	items = items w,
	action = action w,
	stdgen = stdgen w,
	wave = wave w,
	toPick = toPick w,
	store = c,
	worldmap = worldmap w,
	dirs = dirs w
}

changeMon :: Monster -> World -> World
changeMon mon w = World {
	units = (x, y, mon) : (tail $ units w),
	message = message w,
	items = items w,
	action = action w,
	stdgen = stdgen w,
	wave = wave w,
	toPick = toPick w,
	store = store w,
	worldmap = worldmap w,
	dirs = dirs w
} where (x, y, _) = head $ units w

changeMons :: [Unit] -> World -> World
changeMons mons w = World {
	units = mons,
	message = message w,
	items = items w,
	action = action w,
	stdgen = stdgen w,
	wave = wave w,
	toPick = toPick w,
	store = store w,
	worldmap = worldmap w,
	dirs = dirs w
}

addMessage :: String -> World -> World
addMessage s w = World {
	units = units w,
	message = oldMessage w ++ s,
	items = items w,
	action = action w,
	stdgen = stdgen w,
	wave = wave w,
	toPick = toPick w,
	store = store w,
	worldmap = worldmap w,
	dirs = dirs w
}

clearMessage :: World -> World
clearMessage w = World {
	units = units w,
	message = "",
	items = items w,
	action = action w,
	stdgen = stdgen w,
	wave = wave w,
	toPick = toPick w,
	store = store w,
	worldmap = worldmap w,
	dirs = dirs w
}

changeGen :: StdGen -> World -> World
changeGen g w = World {
	units = units w,
	message = message w,
	items = items w,
	action = action w,
	stdgen = g,
	wave = wave w,
	toPick = toPick w,
	store = store w,
	worldmap = worldmap w,
	dirs = dirs w
}

changeCoords :: Monster -> Int -> Int -> Monster
changeCoords m x y = Monster {
	ai = ai m,
	parts = parts m,
	x = x,
	y = y,
	name = name m,
	stddmg = stddmg m,
	inv = inv m,
	slowness = slowness m,
	time = time m
}

changeParts :: Monster -> [Part] -> Monster
changeParts m ps = Monster {
	ai = ai m,
	parts = ps,
	x = x m,
	y = y m,
	name = name m,
	stddmg = stddmg m,
	inv = inv m,
	slowness = slowness m,
	time = time m
}

oldMessage :: World -> String
oldMessage world =
	if msg == "" || last msg == ' '
	then msg
	else msg ++ " "
	where msg = message world
	
tickDownMon :: Monster -> Monster
tickDownMon m = Monster {
	ai = ai m,
	parts = parts m,
	x = x m,
	y = y m,
	name = name m,
	stddmg = stddmg m,
	inv = inv m,
	slowness = slowness m,
	time = time m - 1
}

resetTimeMon :: Monster -> Monster
resetTimeMon m = Monster {
	ai = ai m,
	parts = parts m,
	x = x m,
	y = y m,
	name = name m,
	stddmg = stddmg m,
	inv = inv m,
	slowness = slowness m,
	time = effectiveSlowness m
}


delObj :: Key -> Monster -> Monster
delObj c mon = Monster {
	ai = ai mon,
	parts = parts mon,
	x = x mon,
	y = y mon,
	name = name mon,
	stddmg = stddmg mon,
	inv = newinv,
	slowness = slowness mon,
	time = time mon
} where
	[obj] = filter (\(x,_,_) -> KeyChar x == c) $ inv mon
	newinv = 
		if third obj == 1
		then filter (\(x, _, _) -> KeyChar x /= c) $ inv mon
		else map decCount $ inv mon
	decCount (x, o, n) = 
		if KeyChar x == c
		then (x, o, n - 1)
		else (x, o, n)

decChargeByKey :: Char -> Monster -> Monster
decChargeByKey c mon = Monster {
	ai = ai mon,
	parts = parts mon,
	x = x mon,
	y = y mon,
	name = name mon,
	stddmg = stddmg mon,
	inv = map (\(x, obj,n) -> if x == c then (x, decCharge obj, n) else (x, obj, n)) $ inv mon,
	slowness = slowness mon,
	time = time mon
}

decCharge :: Object -> Object
decCharge obj = Wand {
	title = title obj,
	act = act obj,
	range = range obj,
	charge = charge obj - 1
}


changePickFirst :: Key -> World -> World
changePickFirst c world = World {
	units = units world,
	message = message world,
	items = items world,
	action = action world,
	stdgen = stdgen world,
	wave = wave world,
	toPick = newToPick,
	store = store world,
	worldmap = worldmap world,
	dirs = dirs world
} where
	KeyChar sym = c
	newToPick =
		if member sym $ toPick world
		then delete sym $ toPick world
		else insert sym $ toPick world
		
addItem :: (Int, Int, Object, Int) -> World -> World
addItem i w = World {
	units = units w,
	message = message w,
	items = i : items w,
	action = action w,
	stdgen = stdgen w,
	wave = wave w,
	toPick = toPick w,
	store = store w,
	worldmap = worldmap w,
	dirs = dirs w
}

changeMap :: Int -> Int -> Terrain -> World -> World
changeMap x y t w = World {
	units = units w,
	message = message w,
	items = items w,
	action = action w,
	stdgen = stdgen w,
	wave = wave w,
	toPick = toPick w,
	store = store w,
	worldmap = changeElem2 x y t $ worldmap w,
	dirs = dirs w
}

changeElem :: Int -> a -> [a] -> [a]
changeElem x t ts
	| x == 0 = t : tail ts
	| x > 0 = head ts : changeElem (x - 1) t (tail ts)

changeElem2 :: Int -> Int -> a -> [[a]] -> [[a]]
changeElem2 x y t tss
	| x > 0 = head tss : changeElem2 (x - 1) y t (tail tss)
	| x == 0 = changeElem y t (head tss) : tail tss


