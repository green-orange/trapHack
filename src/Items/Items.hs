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

-- | dir from given char if it's possible
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

-- | quaff a potion with given position in inventory
quaffFirst :: Char -> World -> (World, Bool)
quaffFirst c world
	| not $ hasPart aRM oldMon = (maybeAddMessage 
		(msgNeedArms "quaff a potion") world {action = Move}, False)
	| isNothing objects = 
		(maybeAddMessage msgNoItem world {action = Move}, False)
	| not $ isPotion obj = (maybeAddMessage (msgDontKnow "quaff")
		world {action = Move}, False)
	| otherwise
		= (changeMon mon' $ addNeutralMessage newMsg 
		world {action = Move, stdgen = g}, True) where
	objects = M.lookup c $ inv $ getFirst world
	newMsg = name (getFirst world) ++ " quaff" ++ ending world 
		++ titleShow obj ++ "."
	Just (obj, _) = objects
	oldMon = getFirst world
	(mon, g) = act obj (oldMon, stdgen world)
	mon' = delObj c mon

-- | read a scroll with given position in inventory
readFirst :: Char -> World -> (World, Bool)
readFirst c world 
	| not $ hasPart aRM mon = (maybeAddMessage 
		(msgNeedArms "read a scroll") world {action = Move}, False)
	| isNothing objects =
		(maybeAddMessage msgNoItem world {action = Move}, False)
	| not $ isScroll obj = (maybeAddMessage (msgDontKnow "read")
		world {action = Move}, False)
	| otherwise =
		(changeMon mon' $ addNeutralMessage newMsg 
		newWorld {action = Move}, True) where
	objects = M.lookup c $ inv mon
	newMsg = name mon ++ " read" ++ ending world 
		++ titleShow obj ++ "."
	Just (obj, _) = objects
	newWorld = actw obj world
	mon = getFirst world
	mon' = delObj c $ getFirst newWorld

-- | zap a wand in given direction when item position is lying in 'prevAction'
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
		(changeMon mon newWorld {action = Move}, True) where
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
	failWorld = world {action = Move}

-- | zap a wand from given position in given direction
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
	newMWorld = addMessage (msg, color) world {units' = newMons}
	color = 
		if isPlayerNow world
		then gREEN
		else case isPlayer <$> M.lookup (x, y) (units world) of
			Nothing    -> putWE "zap"
			Just False -> bLUE
			Just True  -> rED

-- | zap a wand with given position in inventory (for AI usage)
zapMon :: Char -> Char -> World -> World
zapMon dir' obj world = fst $ zapFirst dir' $ world {prevAction = obj}

-- | set a trap with given position in inventory
trapFirst :: Char -> World -> (World, Bool)
trapFirst c world
	| not $ isCell x y = putWE "trapFirst"
	| not $ hasPart aRM oldMon =
		(maybeAddMessage (msgNeedArms "set a trap") failWorld, False)
	| isNothing objects = (maybeAddMessage msgNoItem failWorld, False)
	| not $ isTrap obj = (maybeAddMessage msgNotTrap failWorld, False)
	| terrain (worldmap world A.! (x, y)) /= Empty =
		(maybeAddMessage msgTrapOverTrap failWorld, False)
	| otherwise = (addNeutralMessage newMsg $ changeMon mon 
		$ changeTerr x y (num obj) world {action = Move}, True) where
	objects = M.lookup c $ inv $ getFirst world
	x = xFirst world
	y = yFirst world
	oldMon = getFirst world
	Just (obj, _) = objects
	mon = delObj c oldMon
	failWorld = world {action = Move}
	newMsg = name oldMon ++ " set" ++ ending world ++ title obj ++ "."

-- | remove a trap on the cell when you stand
untrapFirst :: World -> (World, Bool)
untrapFirst world 
	| not $ isCell x y = putWE "untrapFirst"
	| not $ hasPart aRM mon =
		(maybeAddMessage (msgNeedArms "remove a trap") failWorld, False)
	| not $ isUntrappable $ worldmap world A.! (x, y) =
		(maybeAddMessage msgCantUntrap failWorld, False)
	| otherwise =
		(addItem (x, y, trap, 1) $ addNeutralMessage newMsg 
		$ changeTerr x y Empty world {action = Move}, True) where
	x = xFirst world
	y = yFirst world
	mon = getFirst world
	failWorld = world {action = Move}
	trap = trapFromCell $ worldmap world A.! (x,y)
	newMsg = name mon ++ " untrap" ++ ending world ++ title trap ++ "."

-- | fire with given direction when missile is lying in 'prevAction'
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
	| otherwise = (newWorld {action = Move}, True) where
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
	failWorld = world {action = Move}

-- | fire from given position with given direction
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
	newWorld = addMessage (msg, color)
		world {units' = insertU (x, y) newMon $ units' world, stdgen = g'}
	color = 
		if isPlayerNow world
		then gREEN
		else case isPlayer <$> M.lookup (x, y) (units world) of
			Nothing    -> putWE "fire"
			Just False -> bLUE
			Just True  -> rED

-- | fire a missile with given position in inventory (for AI usage)
fireMon :: Char -> Char -> World -> World
fireMon dir' obj world = fst $ fireFirst dir' $ world {prevAction = obj}

-- | eat an item with given position in inventory
eatFirst :: Char -> World -> (World, Bool)
eatFirst c world 
	| not $ hasUpperLimb mon = (maybeAddMessage 
		(msgNeedArms "eat") world {action = Move}, False)
	| isNothing objects =
		(maybeAddMessage msgNoItem world {action = Move}, False)
	| not $ isFood obj = (maybeAddMessage (msgDontKnow "eat")
		world {action = Move}, False)
	| otherwise =
		(changeMon mon' $ addNeutralMessage newMsg 
		world {action = Move}, True) where
	objects = M.lookup c $ inv $ getFirst world
	newMsg = name (getFirst world) ++ " eat" ++ ending world 
		++ titleShow obj ++ "."
	Just (obj, _) = objects
	mon = getFirst world
	mon' = delObj c $ changeTemp Nutrition (Just $ nutr + nutrition obj) mon
	Just nutr = temp mon !! fromEnum Nutrition

-- | use an item in given direction when position is lying in 'prevAction'
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
		(newWorld {action = Move}, correct) where
	objects = M.lookup (prevAction world) $ inv $ getFirst world
	Just (dx, dy) = dir c
	(newWorld, correct) = use world x y dx dy obj
	x = xFirst world
	y = yFirst world
	Just (obj, _) = objects
	failWorld = world {action = Move}

-- | use a tool from given position in given direction
use :: World -> Int -> Int -> Int -> Int -> Object -> (World, Bool)
use world x y dx dy obj = case tooltype obj of
	PickAxe -> usePickAxe world x y dx dy obj

-- | use a tool from given position in given direction
usePickAxe :: World -> Int -> Int -> Int -> Int -> Object -> (World, Bool)
usePickAxe world x y dx dy obj
	| not $ isCell (x + dx) (y + dy) = (maybeAddMessage msgNECell world, False)
	| not (isSafeByBounds (-1) 2 world x y dx dy)
		|| height cell == 0
		= (maybeAddMessage msgCantDig world, False)
	| charge obj == 0 =
		(maybeAddMessage (msgNoCharge "pick axe") world, True)
	| not ok = (maybeAddMessage msgFullInv world, False)
	| otherwise = (addNeutralMessage (msgGetStones cnt) 
		$ changeMon newMon 
		world {worldmap = worldmap world A.// [((x + dx, y + dy), 
		cell {height = height cell - 1})], stdgen = g}, True)
	where
		(q, g) = randomR (0.0, 1.0) $ stdgen world
		cnt = inverseSquareRandom q
		cell = worldmap world A.! (x + dx, y + dy)
		mon = getFirst world
		invOld = inv mon
		(invNew, ok) = case addInv (itemFromRes Stone, cnt) invOld of
			Nothing -> (putWE "usePickAxe", False)
			Just i -> (i, True)
		newMon = decChargeByKey (prevAction world) mon {inv = invNew}
	
