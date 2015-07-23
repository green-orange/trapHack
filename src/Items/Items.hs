module Items.Items where

import Data.Const
import Data.World
import Data.Monster
import Data.Define
import Utils.Random
import Utils.Items
import Utils.Changes
import Utils.Stuff
import Utils.HealDamage
import Items.Stuff
import Items.ItemsOverall
import Monsters.Parts
import IO.Messages
import IO.Colors
import IO.Texts

import Data.Maybe (isNothing, isJust, fromJust)
import Control.Applicative ((<$>))
import System.Random (randomR)
import qualified Data.Map as M
import qualified Data.Array as A

dir :: Char -> Maybe (Int, Int)
dir c = case c of
	'k' -> Just ( 0, -1)
	'8' -> Just ( 0, -1)
	'j' -> Just ( 0,  1)
	'2' -> Just ( 0,  1)
	'h' -> Just (-1,  0)
	'4' -> Just (-1,  0)
	'l' -> Just ( 1,  0)
	'6' -> Just ( 1,  0)
	'y' -> Just (-1, -1)
	'7' -> Just (-1, -1)
	'u' -> Just ( 1, -1)
	'9' -> Just ( 1, -1)
	'b' -> Just (-1,  1)
	'1' -> Just (-1,  1)
	'n' -> Just ( 1,  1)
	'3' -> Just ( 1,  1)
	'.' -> Just ( 0,  0)
	'5' -> Just ( 0,  0)
	_           -> Nothing

quaffFirst :: Char -> World -> (World, Bool)
quaffFirst c world
	| not $ hasPart aRM oldMon = (maybeAddMessage 
		(msgNeedArms "quaff a potion") $ changeAction Move world, False)
	| isNothing objects = 
		(maybeAddMessage msgNoItem $ changeAction Move world, False)
	| not $ isPotion obj = (maybeAddMessage (msgDontKnow "quaff")
		$ changeAction Move world, False)
	| otherwise
		= (changeGen g $ changeMon mon' $ addNeutralMessage newMsg 
		$ changeAction Move world, True) where
	objects = M.lookup c $ inv $ getFirst world
	newMsg = name (getFirst world) ++ " quaff" ++ ending world 
		++ titleShow obj ++ "."
	Just (obj, _) = objects
	oldMon = getFirst world
	(mon, g) = act obj (oldMon, stdgen world)
	mon' = delObj c mon
	
readFirst :: Char -> World -> (World, Bool)
readFirst c world 
	| not $ hasPart aRM mon = (maybeAddMessage 
		(msgNeedArms "read a scroll") $ changeAction Move world, False)
	| isNothing objects =
		(maybeAddMessage msgNoItem $ changeAction Move world, False)
	| not $ isScroll obj = (maybeAddMessage (msgDontKnow "read")
		$ changeAction Move world, False)
	| otherwise =
		(changeMon mon' $ addNeutralMessage newMsg 
		$ changeAction Move newWorld, True) where
	objects = M.lookup c $ inv mon
	newMsg = name mon ++ " read" ++ ending world 
		++ titleShow obj ++ "."
	Just (obj, _) = objects
	newWorld = actw obj world
	mon = getFirst world
	mon' = delObj c $ getFirst newWorld

zapFirst :: Char -> World -> (World, Bool)
zapFirst c world 
	| not $ hasPart aRM $ getFirst world =
		(maybeAddMessage (msgNeedArms "zap a wand") failWorld, False)
	| isNothing objects =
		(maybeAddMessage msgNoItem failWorld, False)
	| not $ isWand obj =
		(maybeAddMessage (msgDontKnow "zap") failWorld, False)
	| isNothing $ dir c =
		(maybeAddMessage msgNotDir failWorld, False)
	| charge obj == 0 =
		(maybeAddMessage (msgNoCharge "wand") failWorld, True)
	| otherwise =
		(changeMon mon $ changeAction Move newWorld, True) where
	objects = M.lookup (prevAction world) $ inv $ getFirst world
	Just (dx, dy) = dir c
	newWorld =
		if isCell (x + dx) (y + dy)
		then zap world (x + dx) (y + dy) dx dy obj
		else failWorld
	x = xFirst world
	y = yFirst world
	oldMon = getFirst newWorld
	Just (obj, _) = objects
	mon = decChargeByKey (prevAction newWorld) oldMon
	failWorld = changeAction Move world

zap :: World -> Int -> Int -> Int -> Int -> Object -> World
zap world x y dx dy obj
	| (range obj == 0) || incorrect = world
	| (dx == 0) && (dy == 0) = newMWorld
	| otherwise = zap newMWorld xNew yNew dx dy $ decRange obj where
	(incorrect, xNew, yNew) = 
		if isCell (x + dx) (y + dy)
		then (False, x + dx, y + dy)
		else (True, 0, 0)
	decRange :: Object -> Object
	decRange obj' = obj' {range = range obj - 1}
	(newMons, msg) = 
		case M.lookup (x, y) $ units world of
		Nothing -> (units' world, "")
		Just mon -> (update x y $ (units' world) {list = 
			M.insert (x, y) (fst $ act obj (mon, stdgen world)) $ units world},
			msgWand (title obj) (name mon))
	newMWorld = addMessage (msg, color) $ changeMons newMons world
	color = 
		if isPlayerNow world
		then gREEN
		else case isPlayer <$> M.lookup (x, y) (units world) of
			Nothing    -> lol
			Just False -> bLUE
			Just True  -> rED

zapMon :: Char -> Char -> World -> World
zapMon dir' obj world = fst $ zapFirst dir' $ world {prevAction = obj}
		
trapFirst :: Char -> World -> (World, Bool)
trapFirst c world
	| x < 0 || y < 0 || x > maxX || y > maxY = error $ msgWE "trapFirst"
	| not $ hasPart aRM oldMon =
		(maybeAddMessage (msgNeedArms "set a trap") failWorld, False)
	| isNothing objects = (maybeAddMessage msgNoItem failWorld, False)
	| not $ isTrap obj = (maybeAddMessage msgNotTrap failWorld, False)
	| terrain (worldmap world A.! (x, y)) /= Empty =
		(maybeAddMessage msgTrapOverTrap failWorld, False)
	| otherwise = (addNeutralMessage newMsg $ changeMon mon 
		$ changeTerr x y (num obj) $ changeAction Move world, True) where
	objects = M.lookup c $ inv $ getFirst world
	x = xFirst world
	y = yFirst world
	oldMon = getFirst world
	Just (obj, _) = objects
	mon = delObj c oldMon
	failWorld = changeAction Move world
	newMsg = name oldMon ++ " set" ++ ending world ++ title obj ++ "."
	
untrapFirst :: World -> (World, Bool)
untrapFirst world 
	| x < 0 || y < 0 || x > maxX || y > maxY = error $ msgWE "untrapFirst"
	| not $ hasPart aRM mon =
		(maybeAddMessage (msgNeedArms "remove a trap") failWorld, False)
	| not $ isUntrappable $ worldmap world A.! (x, y) =
		(maybeAddMessage msgCantUntrap failWorld, False)
	| otherwise =
		(addItem (x, y, trap, 1) $ addNeutralMessage newMsg 
		$ changeTerr x y Empty $ changeAction Move world, True) where
	x = xFirst world
	y = yFirst world
	mon = getFirst world
	failWorld = changeAction Move world
	trap = trapFromCell $ worldmap world A.! (x,y)
	newMsg = name mon ++ " untrap" ++ ending world ++ title trap ++ "."
	
fireFirst :: Char -> World -> (World, Bool)
fireFirst c world
	| not $ hasPart aRM oldMon =
		(maybeAddMessage (msgNeedArms "fire") failWorld, False)
	| isNothing objects =
		(maybeAddMessage msgNoItem failWorld, False)
	| not $ isMissile obj =
		(maybeAddMessage (msgDontKnow "fire") failWorld, False)
	| null intended =
		(maybeAddMessage msgNoWeapAppMiss failWorld, False)
	| isNothing $ dir c =
		(maybeAddMessage msgNotDir failWorld, False)
	| otherwise = (changeAction Move newWorld, True) where
	objects = M.lookup (prevAction world) $ inv oldMon
	intended = filter (\w -> isLauncher w && launcher obj == category w) listWield
	listWield = fst . fromJust <$> filter isJust 
		((flip M.lookup (inv oldMon) . (\ p -> objectKeys p 
		!! fromEnum WeaponSlot)) <$> filter isUpperLimb (parts oldMon))
	x = xFirst world
	y = yFirst world
	oldMon = getFirst world
	cnt = min n $ sum $ count <$> intended
	newWorld = 
		if isCell (x + dx) (y + dy)
		then foldr (.) id (replicate cnt $ fire (x + dx) (y + dy) dx dy obj)
			$ changeMon (fulldel oldMon) world
		else failWorld
	Just (dx, dy) = dir c
	Just (obj, n) = objects
	fulldel = foldr (.) id $ replicate cnt $ delObj $ prevAction world
	failWorld = changeAction Move world
	
fire :: Int -> Int -> Int -> Int -> Object -> World -> World
fire x y dx dy obj world
	| incorrect = world
	| isNothing maybeMon = fire xNew yNew dx dy obj world
	| otherwise = newWorld where
	maybeMon = M.lookup (x, y) $ units world 
	Just mon = maybeMon
	(incorrect, xNew, yNew) =
		if isCell (x + dx) (y + dy)
		then (False, x + dx, y + dy)
		else (True, 0, 0)
	(newDmg, g) = objdmg obj world
	(newMon, g') = dmgRandom newDmg mon g
	msg = case newDmg of
		Nothing -> capitalize (title obj) ++ msgMiss
		Just _ -> capitalize (title obj) ++ msgHitMissile ++ name mon ++ "."
	newWorld = addMessage (msg, color) $ changeGen g' 
		$ changeMons (insertU (x, y) newMon $ units' world) world
	color = 
		if isPlayerNow world
		then gREEN
		else case isPlayer <$> M.lookup (x, y) (units world) of
			Nothing    -> lol
			Just False -> bLUE
			Just True  -> rED
		
fireMon :: Char -> Char -> World -> World
fireMon dir' obj world = fst $ fireFirst dir' $ world {prevAction = obj}

eatFirst :: Char -> World -> (World, Bool)
eatFirst c world 
	| not $ hasUpperLimb mon = (maybeAddMessage 
		(msgNeedArms "eat") $ changeAction Move world, False)
	| isNothing objects =
		(maybeAddMessage msgNoItem $ changeAction Move world, False)
	| not $ isFood obj = (maybeAddMessage (msgDontKnow "eat")
		$ changeAction Move world, False)
	| otherwise =
		(changeMon mon' $ addNeutralMessage newMsg 
		$ changeAction Move world, True) where
	objects = M.lookup c $ inv $ getFirst world
	newMsg = name (getFirst world) ++ " eat" ++ ending world 
		++ titleShow obj ++ "."
	Just (obj, _) = objects
	mon = getFirst world
	mon' = delObj c $ changeTemp Nutrition (Just $ nutr + nutrition obj) mon
	Just nutr = temp mon !! fromEnum Nutrition

useFirst :: Char -> World -> (World, Bool)
useFirst c world
	| not $ hasPart aRM $ getFirst world =
		(maybeAddMessage (msgNeedArms "use a tool") failWorld, False)
	| isNothing objects =
		(maybeAddMessage msgNoItem failWorld, False)
	| not $ isTool obj =
		(maybeAddMessage (msgDontKnow "use") failWorld, False)
	| isNothing $ dir c =
		(maybeAddMessage msgNotDir failWorld, False)
	| not $ isCell (x + dx) (y + dy) = 
		(maybeAddMessage msgNECell failWorld, False)
	| otherwise =
		(changeAction Move newWorld, correct) where
	objects = M.lookup (prevAction world) $ inv $ getFirst world
	Just (dx, dy) = dir c
	(newWorld, correct) = use world x y dx dy obj
	x = xFirst world
	y = yFirst world
	Just (obj, _) = objects
	failWorld = changeAction Move world

use :: World -> Int -> Int -> Int -> Int -> Object -> (World, Bool)
use world x y dx dy obj = case tooltype obj of
	PickAxe -> usePickAxe world x y dx dy obj

usePickAxe :: World -> Int -> Int -> Int -> Int -> Object -> (World, Bool)
usePickAxe world x y dx dy obj
	| x + dx < 0 || y + dy < 0 || x + dx > maxX || y + dy > maxY
		= (maybeAddMessage msgNECell world, False)
	| not (isSafeByBounds (-1) 2 world x y dx dy)
		|| height cell == 0
		= (maybeAddMessage msgCantDig world, False)
	| charge obj == 0 =
		(maybeAddMessage (msgNoCharge "pick axe") world, True)
	| not ok = (maybeAddMessage msgFullInv world, False)
	| otherwise = (addNeutralMessage (msgGetStones cnt) 
		$ changeGen g $ changeMon newMon 
		world {worldmap = worldmap world A.// [((x + dx, y + dy), 
		cell {height = height cell - 1})]}, True)
	where
		(q, g) = randomR (0.0, 1.0) $ stdgen world
		cnt = inverseSquareRandom q
		cell = worldmap world A.! (x + dx, y + dy)
		mon = getFirst world
		invOld = inv mon
		(invNew, ok) = case addInv (itemFromRes Stone, cnt) invOld of
			Nothing -> (lol, False)
			Just i -> (i, True)
		newMon = decChargeByKey (prevAction world) mon {inv = invNew}
	
