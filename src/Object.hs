module Object where

import Data
import Changes
import Stuff
import Utils4stuff
import HealDamage
import Messages
import Utils4objects
import Parts
import Utils4mon
import Colors
import Texts

import UI.HSCurses.Curses (Key(..))
import Data.Maybe (isNothing, isJust, fromJust)
import qualified Data.Map as M
import qualified Data.Array as A

dir :: Key -> Maybe (Int, Int)
dir c = case c of
	KeyChar 'k' -> Just ( 0, -1)
	KeyChar '8' -> Just ( 0, -1)
	KeyChar 'j' -> Just ( 0,  1)
	KeyChar '2' -> Just ( 0,  1)
	KeyChar 'h' -> Just (-1,  0)
	KeyChar '4' -> Just (-1,  0)
	KeyChar 'l' -> Just ( 1,  0)
	KeyChar '6' -> Just ( 1,  0)
	KeyChar 'y' -> Just (-1, -1)
	KeyChar '7' -> Just (-1, -1)
	KeyChar 'u' -> Just ( 1, -1)
	KeyChar '9' -> Just ( 1, -1)
	KeyChar 'b' -> Just (-1,  1)
	KeyChar '1' -> Just (-1,  1)
	KeyChar 'n' -> Just ( 1,  1)
	KeyChar '3' -> Just ( 1,  1)
	KeyChar '.' -> Just ( 0,  0)
	KeyChar '5' -> Just ( 0,  0)
	_           -> Nothing

quaffFirst :: Key -> World -> (World, Bool)
quaffFirst c world
	| not $ hasPart aRM oldMon = (maybeAddMessage 
		(msgNeedArms "quaff a potion") $ changeAction ' ' world, False)
	| isNothing objects = 
		(maybeAddMessage msgNoItem $ changeAction ' ' world, False)
	| not $ isPotion obj = (maybeAddMessage (msgDontKnow "quaff")
		$ changeAction ' ' world, False)
	| otherwise
		= (changeGen g $ changeMon mon' $ addNeutralMessage newMsg 
		$ changeAction ' ' world, True) where
	objects = M.lookup (fromKey c) $ inv $ getFirst world
	newMsg = name (getFirst world) ++ " quaff" ++ ending world 
		++ titleShow obj ++ "."
	(obj, _) = fromJust objects
	oldMon = getFirst world
	(mon, g) = act obj (oldMon, stdgen world)
	mon' = delObj c mon
	
readFirst :: Key -> World -> (World, Bool)
readFirst c world 
	| not $ hasPart aRM mon = (maybeAddMessage 
		(msgNeedArms "read a scroll") $ changeAction ' ' world, False)
	| isNothing objects =
		(maybeAddMessage msgNoItem $ changeAction ' ' world, False)
	| not $ isScroll obj = (maybeAddMessage (msgDontKnow "read")
		$ changeAction ' ' world, False)
	| otherwise =
		(changeMon mon' $ addNeutralMessage newMsg 
		$ changeAction ' ' newWorld, True) where
	objects = M.lookup (fromKey c) $ inv $ getFirst world
	newMsg = name (getFirst world) ++ " read" ++ ending world 
		++ titleShow obj ++ "."
	(obj, _) = fromJust objects
	newWorld = actw obj world
	mon = getFirst world
	mon' = delObj c mon

zapFirst :: Key -> World -> (World, Bool)
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
		(maybeAddMessage msgNoCharge failWorld, True)
	| otherwise =
		(changeMon mon $ changeAction ' ' newWorld, True) where
	objects = M.lookup (prevAction world) $ inv $ getFirst world
	(dx, dy) = fromJust $ dir c
	maybeCoords = dirs world (x, y, dx, dy)
	newWorld = case maybeCoords of
		Just (xNew, yNew) -> zap world xNew yNew dx dy obj
		Nothing -> failWorld
	x = xFirst world
	y = yFirst world
	oldMon = getFirst newWorld
	(obj, _) = fromJust objects
	mon = decChargeByKey (prevAction newWorld) oldMon
	failWorld = changeAction ' ' world

zap :: World -> Int -> Int -> Int -> Int -> Object -> World
zap world x y dx dy obj
	| (range obj == 0) || incorrect = world
	| (dx == 0) && (dy == 0) = newMWorld
	| otherwise = zap newMWorld xNew yNew dx dy $ decRange obj where
	(incorrect, (xNew, yNew)) = case dirs world (x, y, dx, dy) of
		Nothing -> (True, (0, 0))
		Just p -> (False, p)
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
		else case fmap isPlayer $ M.lookup (x, y) $ units world of
		Nothing    -> lol
		Just False -> bLUE
		Just True  -> rED

zapMon :: Key -> Char -> World -> World
zapMon dir' obj world = fst $ zapFirst dir' $ world {prevAction = obj}
		
trapFirst :: Key -> World -> (World, Bool)
trapFirst c world
	| not $ hasPart aRM oldMon =
		(maybeAddMessage (msgNeedArms "set a trap") failWorld, False)
	| isNothing objects = (maybeAddMessage msgNoItem failWorld, False)
	| not $ isTrap obj = (maybeAddMessage msgNotTrap failWorld, False)
	| worldmap world A.! (x, y) /= Empty =
		(maybeAddMessage msgTrapOverTrap failWorld, False)
	| otherwise = (addNeutralMessage newMsg $ changeMon mon 
		$ changeMap x y (num obj) $ changeAction ' ' world, True) where
	objects = M.lookup (fromKey c) $ inv $ getFirst world
	x = xFirst world
	y = yFirst world
	oldMon = getFirst world
	(obj, _) = fromJust objects
	mon = delObj c oldMon
	failWorld = changeAction ' ' world
	newMsg = name oldMon ++ " set" ++ ending world ++ title obj ++ "."
	
untrapFirst :: World -> (World, Bool)
untrapFirst world 
	| not $ hasPart aRM mon =
		(maybeAddMessage (msgNeedArms "remove a trap") failWorld, False)
	| not $ isUntrappable $ worldmap world A.! (x, y) =
		(maybeAddMessage msgCantUntrap failWorld, False)
	| otherwise =
		(addItem (x, y, trap, 1) $ addNeutralMessage newMsg 
		$ changeMap x y Empty $ changeAction ' ' world, True) where
	x = xFirst world
	y = yFirst world
	mon = getFirst world
	failWorld = changeAction ' ' world
	trap = trapFromTerrain $ worldmap world A.! (x,y)
	newMsg = name mon ++ " untrap" ++ ending world ++ title trap ++ "."
	
fireFirst :: Key -> World -> (World, Bool)
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
	| otherwise = (changeAction ' ' newWorld, True) where
	objects = M.lookup (prevAction world) $ inv oldMon
	intended = filter (\w -> isLauncher w && launcher obj == category w) listWield
	listWield = map (fst . fromJust) $ filter isJust 
		$ map (flip M.lookup (inv oldMon) . (\ p -> objectKeys p 
		!! fromEnum WeaponSlot)) $ filter isUpperLimb $ parts oldMon
	x = xFirst world
	y = yFirst world
	oldMon = getFirst world
	maybeCoords = dirs world (x, y, dx, dy)
	cnt = min n $ sum $ map count intended
	newWorld = case maybeCoords of
		Just (xNew, yNew) -> foldr (.) id (replicate cnt $ 
			fire xNew yNew dx dy obj) $ changeMon (fulldel oldMon) world
		Nothing -> failWorld
	Just (dx, dy) = dir c
	(obj, n) = fromJust objects
	fulldel = foldr (.) id $ replicate cnt $ delObj $ KeyChar $ prevAction world
	failWorld = changeAction ' ' world
	
fire :: Int -> Int -> Int -> Int -> Object -> World -> World
fire x y dx dy obj world
	| incorrect = world
	| isNothing maybeMon = fire xNew yNew dx dy obj world
	| otherwise = newWorld where
	maybeMon = M.lookup (x, y) $ units world 
	Just mon = maybeMon
	(incorrect, (xNew, yNew)) = case dirs world (x, y, dx, dy) of
		Nothing -> (True, (0, 0))
		Just p -> (False, p)
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
		else case fmap isPlayer $ M.lookup (x, y) $ units world of
		Nothing    -> lol
		Just False -> bLUE
		Just True  -> rED
		
fireMon :: Key -> Char -> World -> World
fireMon dir' obj world = fst $ fireFirst dir' $ world {prevAction = obj}
