module Show where

import Data
import Parts
import Messages
import ObjectOverall
import Utils4objects
import Colors

import UI.HSCurses.Curses
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Array as A
import Control.Monad (unless)

shiftRightHP, shiftAttrs, shiftW, shiftA, shiftJ, diff :: Int
shiftRightHP = 2 * xSight + 5
shiftAttrs = 2 * xSight + 30
diff = 20
shiftW = 30
shiftA = shiftW + diff
shiftJ = shiftA + diff

castEnum :: Char -> ChType
castEnum = toEnum . fromEnum

placeChar :: Int -> Int -> Char -> IO ()
placeChar x y = mvAddCh (y + shiftDown + ySight) (x + xSight)
	. castEnum

drawUnit :: World -> ((Int, Int), Monster) -> IO ()
drawUnit world ((x, y), mon) =
	unless (abs dx > xSight || abs dy > ySight) $ do
		wAttrSet stdScr (attr, Pair $ back + color' - 8)
		placeChar dx dy sym where
		attr = 
			if x == xFirst world && y == yFirst world ||
				action world == '?' && x == xInfo world && y == yInfo world
			then setStandout attr0 True
			else attr0
		(sym, color') = symbolMon $ name mon
		back = colorFromTerr $ worldmap world A.! (x,y)
		dx = x - xFirst world
		dy = y - yFirst world

drawCell :: World -> ((Int, Int), Terrain) -> IO ()
drawCell world ((x, y), _) = 
	unless (abs dx > xSight || abs dy > ySight) $ do
		wAttrSet stdScr (attr, Pair $ colorFromTerr $ worldmap world A.! (x,y))
		placeChar dx dy sym where
		attr = 
			if action world == '?' && x == xInfo world && y == yInfo world
			then setStandout attr0 True
			else attr0
		dx = x - xFirst world
		dy = y - yFirst world
		sym = if x == div maxX 2 && y == div maxY 2 then '*' else '.'

drawItem :: World -> (Int, Int, Object, Int) -> IO ()
drawItem world(x, y, item, _) = 
	unless (abs dx > xSight || abs dy > ySight) $ do
		wAttrSet stdScr (attr, Pair $ colorFromTerr $ worldmap world A.! (x,y))
		placeChar dx dy $ symbolItem item where
		dx = x - xFirst world
		dy = y - yFirst world
		attr = 
			if action world == '?' && x == xInfo world && y == yInfo world
			then setStandout attr0 True
			else attr0

showItemsPD :: Int -> S.Set Char -> (Int, Char, (Object, Int)) -> IO ()
showItemsPD h toPick' (n, c, (obj,cnt)) =
	mvWAddStr stdScr (mod (n + 1) h) (30 * div (n + 1) h)
		([c] ++ sym ++ show cnt ++ " * " ++ titleShow obj) where
	sym =
		if S.member c toPick'
		then " + "
		else " - "
		
showMessages :: Int -> [(String, Int)] -> IO ()
showMessages width msgs = 
	unless (sum (map ((1+) . length . fst) msgs) < shiftDown * width - 2)
	(mvWAddStr stdScr (shiftDown - 1) 0 "--MORE--") >>
	foldl (>>=) (return (0, 0)) (map showMessage msgs) >> return ()

showMessage :: (String, Int) -> (Int, Int) -> IO (Int, Int)
showMessage (msg, color') (x, y) = do
	(_, w) <- scrSize
	if fst (rez w) < shiftDown - 1
	then do
		wAttrSet stdScr (attr0, Pair color')
		mvWAddStr stdScr x y msg
		return $ rez w
	else return (x, y)
	where
		rez w = (dx, dy) where
			dy' = y + 1 + length msg
			dx = x + div dy' w
			dy = mod dy' w

showElemRes :: World -> Elem -> IO ()
showElemRes world e =
	if value == 0
	then doNothing
	else mvWAddStr stdScr (shiftDown + 2 + pos) shiftAttrs str where
	pos = fromEnum e
	value = res (getFirst world) !! pos
	str = show e ++ " res: " ++ show value

showIntr :: World -> Intr -> IO ()
showIntr world i = 
	if value == 0
	then doNothing
	else mvWAddStr stdScr (shiftDown + 3 + elems + pos) shiftAttrs str where
	elems = fromEnum (maxBound :: Elem) - fromEnum (minBound :: Elem)
	pos = fromEnum i
	value = intr (getFirst world) !! pos
	str = show i ++ ": " ++ show value

showTemp :: World -> Temp -> IO ()
showTemp world t = 
	case value of
	Nothing -> doNothing
	_ -> mvWAddStr stdScr (shiftDown + 4 + elems + intrs + pos) shiftAttrs str
	where
	elems = fromEnum (maxBound :: Elem) - fromEnum (minBound :: Elem)
	intrs = fromEnum (maxBound :: Intr) - fromEnum (minBound :: Intr)
	pos = fromEnum t
	value = temp (getFirst world) !! pos
	Just rez = value
	str = show t ++ " (" ++ show rez ++ ")"

drawInventory :: World -> Int -> IO ()
drawInventory world h = do
	wAttrSet stdScr (attr0, Pair dEFAULT)
	mvWAddStr stdScr 0 0 "Your inventory: (press Enter or Space to close it)"
	foldl (>>) doNothing $ map showInv stringsToShow where
		items' = M.toList $ inv $ getFirst world
		stringsToShow = zip [1..] $ map (\(c, (obj, n)) -> 
			[c] ++ " - " ++ show n ++ " * " ++ titleShow obj ++
			(if isExistingBindingFirst world c then " (is used)" else "")) items'
		showInv :: (Int, String) -> IO ()
		showInv (n, s) = mvWAddStr stdScr ((+) 1 $ mod n $ h-1) (30 * div n (h-1)) s

drawEquipMenu :: World -> Int -> IO ()
drawEquipMenu world h = do
	wAttrSet stdScr (attr0, Pair dEFAULT)
	mvWAddStr stdScr 0 0 "Change an item or press - to change nothing"
	foldl (>>) doNothing $ map showInv stringsToShow where
		items' = filter (\(c, (obj, _)) -> (binds obj knd == Just (slot world)) 
			&& not (isExistingBindingFirst world c)) $ M.toList $ inv $ getFirst world
		stringsToShow = zip [1..] $ map (\(c, (obj, n)) -> 
			[c] ++ " - " ++ show n ++ " * " ++ titleShow obj) items'
		showInv :: (Int, String) -> IO ()
		showInv (n, s) = mvWAddStr stdScr ((+) 1 $ mod n $ h-1) (30 * div n (h-1)) s
		knd = kind $ parts (getFirst world) !! shift world
		
drawPickOrDrop :: Bool -> World -> Int -> IO ()
drawPickOrDrop isPick world h = do
	wAttrSet stdScr (attr0, Pair dEFAULT)
	mvWAddStr stdScr 0 0 $ "What do you want to " ++ word 
		++ " up? (press Enter to finish)"
	foldl (>>) doNothing $ map (showItemsPD h $ chars world) toShow where
		xNow = xFirst world
		yNow = yFirst world
		toShow =
			if action world == ','
			then zip3 [0..] alphabet $ map (\(_,_,a,b) -> (a,b)) $
				filter (\(x,y,_,_) -> x == xNow && y == yNow) $ items world
			else zipWith (\n (c,x) -> (n,c,x)) [0..] $ M.toList $ inv $ getFirst world 
		word = if isPick then "pick" else "drop"

drawPartChange :: World -> Int -> IO ()
drawPartChange world _ = do
	wAttrSet stdScr (attr0, Pair dEFAULT)
	mvWAddStr stdScr 0 0 "Change your part to bind."
	mvWAddStr stdScr 0 shiftW "Weapon"
	mvWAddStr stdScr 0 shiftA "Armor"
	mvWAddStr stdScr 0 shiftJ "Jewelry"
	foldl (>>) doNothing $ zipWith3 ($) (map (drawPartFull True 1) [1..])
		(repeat $ getFirst world) $ parts $ getFirst world
	wAttrSet stdScr (setBold attr0 True, Pair dEFAULT)
	mvAddCh (shift world + 1) (diff * fromEnum (slot world) + shiftW - 1) $ castEnum '>'

drawJustWorld :: World -> Int -> IO ()
drawJustWorld world _ = do
	wAttrSet stdScr (attr0, Pair dEFAULT)
	(_, w) <- scrSize
	showMessages w $ message world
	foldl (>>) doNothing $ map (drawCell world) $ A.assocs $ worldmap world
	foldl (>>) doNothing $ map (drawItem world) $ items world
	foldl (>>) doNothing $ map (drawUnit world) $ M.toList $ units world
	foldl (>>) doNothing $ zipWith3 ($) (map (drawPart False) [0..])
		(repeat $ getFirst world) $ sortBy (on compare kind) 
		$ parts $ getFirst world
	wAttrSet stdScr (attr0, Pair dEFAULT)
	mvWAddStr stdScr shiftDown shiftAttrs $ "Slowness: " ++ 
		show (effectiveSlowness $ getFirst world)
	mvWAddStr stdScr (shiftDown + 1) shiftAttrs 
			$ "Next wave: " ++ show (wave world)
	wAttrSet stdScr (attr0, Pair rED)
	wAttrSet stdScr (attr0, Pair dEFAULT)
	foldl (>>) doNothing $ map (showElemRes world) [minBound :: Elem .. maxBound :: Elem]
	foldl (>>) doNothing $ map (showIntr world) [minBound :: Intr .. maxBound :: Intr]
	foldl (>>) doNothing $ map (showTemp world) [minBound :: Temp .. maxBound :: Temp]

draw :: World -> IO ()
draw world = do
	(h, _) <- scrSize
	(case action world of 
		'i' -> drawInventory
		'e' -> drawEquipMenu
		',' -> drawPickOrDrop True
		'D' -> drawPickOrDrop False
		'E' -> drawPartChange
		_   -> drawJustWorld) world h
		
				
redraw :: World -> IO Key
redraw world = 
	erase >> draw world >> refresh >> getCh

drawPart :: Bool -> Int -> Monster -> Part -> IO ()
drawPart isFull y = drawPartFull isFull shiftRightHP $ shiftDown + y

drawPartFull :: Bool -> Int -> Int -> Monster -> Part -> IO ()
drawPartFull isFull x y mon part = do
	wAttrSet stdScr (attr0, Pair gREEN)
	if hp part * 3 < maxhp part * 2 || hp part <= 10
	then wAttrSet stdScr (attr0, Pair yELLOW)
	else doNothing
	if hp part * 3 < maxhp part || hp part <= 5
	then wAttrSet stdScr (attr0, Pair rED)
	else doNothing
	if hp part * 8 < maxhp part || hp part <= 3
	then wAttrSet stdScr (attr0, Pair rEDiNVERSE)
	else doNothing
	mvWAddStr stdScr y x     str1
	mvWAddStr stdScr y (x+5) str2
	mvWAddStr stdScr y (x+shiftW-1) strW
	mvWAddStr stdScr y (x+shiftA-1) strA
	mvWAddStr stdScr y (x+shiftJ-1) strJ
	where
		str1 = partToStr (kind part) ++ ":"
		str2 = show (hp part) ++ "/" ++ show (maxhp part) ++
			" rv: " ++ show (regVel part)
		(strW, strA, strJ) =
			if isFull
			then (case objsW of
				Nothing -> ""
				Just (obj,_) -> titleShow obj,
				case objsA of
				Nothing -> ""
				Just (obj,_) -> titleShow obj,
				case objsJ of
				Nothing -> ""
				Just (obj,_) -> titleShow obj)
			else ("","","")
		objsW = M.lookup (objectKeys part !! fromEnum WeaponSlot) $ inv mon
		objsA = M.lookup (objectKeys part !! fromEnum ArmorSlot) $ inv mon
		objsJ = M.lookup (objectKeys part !! fromEnum JewelrySlot) $ inv mon

symbolItem :: Object -> Char
symbolItem (Potion {})   = '!'
symbolItem (Scroll {})   = '?'
symbolItem (Wand {})     = '/'
symbolItem (Trap {})     = '^'
symbolItem (Missile {})  = ']'
symbolItem (Weapon {})   = ')'
symbolItem (Launcher {}) = '}'
symbolItem (Armor {})    = '['
symbolItem (Jewelry {})  = '='
symbolItem _             = error "unknown object"

