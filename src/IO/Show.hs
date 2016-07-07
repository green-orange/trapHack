module IO.Show where

import Data.Const
import Data.World
import Data.Monster
import Data.Define
import Utils.Items
import Utils.Monsters
import Utils.Stuff (isUntrappable)
import Items.ItemsOverall
import Items.Craft
import Monsters.Parts
import IO.Messages
import IO.Colors
import IO.Texts

import UI.HSCurses.Curses
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Array as A
import Control.Monad (unless, when, zipWithM_, (>=>))
import Data.Functor ((<$>))

shiftRightHP, shiftAttrs, shiftW, shiftA, shiftJ, diff, shiftElem, invColumn :: Int
-- | shift from left side to a position with hit points
shiftRightHP = 2 * xSight + 5
-- | shift from left side to a position with some attributes
shiftAttrs = 2 * xSight + 30
-- | difference between of columns in equip menu
diff = 20
-- | 'WeaponSlot' column menu
shiftW = 30
-- | 'ArmorSlot' column menu
shiftA = shiftW + diff
-- | 'JewelrySlot' column menu
shiftJ = shiftA + diff
-- | vertical shift from top to a first 'Elem' position
shiftElem = 4
-- | width of the column in Inventory menu
invColumn = 50

-- | cast 'Char' type to HSCurses 'ChType'  
castEnum :: Char -> ChType
castEnum = toEnum . fromEnum
-- | place given char by given coords
placeChar :: Int -> Int -> Char -> IO ()
placeChar x y = mvAddCh (y + shiftDown + ySight) (x + xSight) . castEnum

type DataToShow = Maybe (Int, Int, Char, Attr, Int, Int)

-- | show something by data: (dx, dx, symbol, attribute, foreground color, background color) 
showByData :: DataToShow -> IO ()
showByData d = case d of
	Nothing -> return ()
	Just (dx, dy, sym, attr, colorFore, colorBack) -> do
		wAttrSet stdScr (attr, Pair $ colorFore + colorBack - 8)
		placeChar dx dy sym

-- | return data to show a unit
dataToShowUnit :: World -> ((Int, Int), Monster) -> DataToShow
dataToShowUnit world ((x, y), mon) =
	if isCell x y
	then
		if abs dx > xSight || abs dy > ySight then Nothing
		else Just (dx, dy, sym, attr, colorFore, defaultBack)
	else putWE "drawUnit"
	where
		attr
			| x == xFirst world && y == yFirst world = setStandout attr0 True
			| showMode world == ColorHeight || 
				showMode world == ColorHeightAbs = setBold attr0 True
			| otherwise = attr0
		(sym, colorFore) = symbolMon $ idM mon
		dx = x - xFirst world
		dy = y - yFirst world

-- | return data to show a cell
dataToShowCell :: World -> ((Int, Int), Cell) -> DataToShow
dataToShowCell world ((x, y), _) = 
	if isCell x y
	then 
		if abs dx > xSight || abs dy > ySight then Nothing
		else Just (dx, dy, sym, attr, colorFore, colorBack)
	else putWE "drawCell"
	where
		attr
			| action world == Info && x == xInfo world && y == yInfo world
				= setStandout attr0 True
			| terrain cell /= Empty = setBold attr0 True
			| otherwise = attr0
		dx = x - xFirst world
		dy = y - yFirst world
		cell = worldmap world A.! (x,y)
		colorBack = 
			if terrain cell /= Empty
			then colorFromCell cell
			else defaultBack
		colorFore = case showMode world of
			ColorHeight -> colorFromHei $ height cell - 
				height (worldmap world A.! (xFirst world, yFirst world))
			ColorHeightAbs -> colorFromHeiAbs $ height cell
			_ -> defaultc
		sym
			| x == div maxX 2 && y == div maxY 2 = '*'
			| isUntrappable $ worldmap world A.! (x,y) = '^'
			| showMode world == NoHeight = '.'
			| otherwise = head $ show $ height $ worldmap world A.! (x,y)

-- | return data to show an item
dataToShowItem :: World -> (Int, Int, Object, Int) -> DataToShow
dataToShowItem world(x, y, item, _) = 
	if isCell x y
	then 
		if abs dx > xSight || abs dy > ySight then Nothing
		else Just (dx, dy, symbolItem item, setBold attr0 True, defaultc, defaultBack)
	else putWE "drawItem"
	where
		dx = x - xFirst world
		dy = y - yFirst world

-- | draw pick or drop menu with given height of screen, set of chosen chars
-- and item on the ground 
showItemsPD :: Int -> S.Set Char -> (Int, Char, (Object, Int)) -> IO ()
showItemsPD h toPick' (n, c, (obj,cnt)) =
	mvWAddStr stdScr (mod (n + 1) h) (30 * div (n + 1) h)
		([c] ++ sym ++ show cnt ++ " * " ++ titleShow obj) where
	sym =
		if S.member c toPick'
		then " + "
		else " - "
-- | show messages in the top of the screen
showMessages :: Int -> [(String, Int)] -> IO ()
showMessages width msgs = 
	unless (sum ((1+) . length . fst <$> msgs) < (shiftDown - 1) * width)
	(mvWAddStr stdScr (shiftDown - 1) 0 msgMore) >>
	foldr ((>=>) . showMessage) return msgs (0, 0) >> return ()
-- | show one message
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
-- | show one elemental resistance
showElemRes :: World -> Elem -> IO ()
showElemRes world e =
	unless (value == 0) $ mvWAddStr stdScr 
		(shiftDown + shiftElem + pos) shiftAttrs str where
	pos = fromEnum e
	value = res (getFirst world) !! pos
	str = show e ++ " res: " ++ show value
-- | show one intrinsic
showIntr :: World -> Intr -> IO ()
showIntr world i = 
	unless (value == 0) $ mvWAddStr stdScr 
		(shiftDown + shiftElem + 1 + elems + pos) shiftAttrs str where
	elems = fromEnum (maxBound :: Elem) - fromEnum (minBound :: Elem)
	pos = fromEnum i
	value = intr (getFirst world) !! pos
	str = show i ++ ": " ++ show value
-- | show one temporary effect
showTemp :: World -> Temp -> IO ()
showTemp world t = 
	case value of
		Nothing -> return ()
		Just rez -> let
				str = show t ++ " (" ++ show rez ++ ")"
				clr = colorFromTemp t rez
			in wAttrSet stdScr (attr0, Pair clr) >>
				mvWAddStr stdScr (shiftDown + shiftElem + 2 + elems
				+ intrs + pos) shiftAttrs str
	where
	elems = fromEnum (maxBound :: Elem) - fromEnum (minBound :: Elem)
	intrs = fromEnum (maxBound :: Intr) - fromEnum (minBound :: Intr)
	pos = fromEnum t
	value = temp (getFirst world) !! pos

-- | show one craft recipe
showRecipe :: Inv -> Recipe -> Int -> IO ()
showRecipe inv' (ress, rez) y = 
	wAttrSet stdScr (attr0, Pair $ colorFromRecipe inv' ress) >>
	mvWAddStr stdScr y 0 str where
	str = toEnum (fromEnum 'a' + y - 1) : " - " 
		++ concatMap resToStr ress ++ "=> " ++ title rez
	resToStr (r, cnt) = show cnt ++ " * " ++ show r ++ " "

-- | draw player inventory
drawInventory :: World -> Int -> IO ()
drawInventory world h = do
	wAttrSet stdScr (attr0, Pair defaultc)
	mvWAddStr stdScr 0 0 msgHeaderInv
	mapM_ showInv stringsToShow where
		items' = M.toList $ inv $ getFirst world
		stringsToShow = zip [1..] $ (\(c, (obj, n)) -> 
			[c] ++ " - " ++ show n ++ " * " ++ titleShow obj ++
			(if isExistingBindingFirst world c then " (is used)" else "")) <$> items'
		showInv :: (Int, String) -> IO ()
		showInv (n, s) = mvWAddStr stdScr ((+) 1 $ mod n $ h-1) (invColumn * div n (h-1)) s
-- | draw list of items in equipment menu
drawEquipMenu :: World -> Int -> IO ()
drawEquipMenu world h = do
	wAttrSet stdScr (attr0, Pair defaultc)
	mvWAddStr stdScr 0 0 msgHeaderEquip
	mapM_ showInv stringsToShow where
		items' = filter (\(c, (obj, _)) -> (binds obj knd == Just (slot world)) 
			&& not (isExistingBindingFirst world c)) $ M.toList $ inv $ getFirst world
		stringsToShow = zip [1..] $ (\(c, (obj, n)) -> 
			[c] ++ " - " ++ show n ++ " * " ++ titleShow obj) <$> items'
		showInv :: (Int, String) -> IO ()
		showInv (n, s) = mvWAddStr stdScr ((+) 1 $ mod n $ h-1) (30 * div n (h-1)) s
		knd = kind $ parts (getFirst world) !! shift world
-- | draw pick or drop menu (first argument is True iff pick)
drawPickOrDrop :: Bool -> World -> Int -> IO ()
drawPickOrDrop isPick world h = do
	wAttrSet stdScr (attr0, Pair defaultc)
	mvWAddStr stdScr 0 0 $ msgHeaderPickDrop word
	mapM_ (showItemsPD h $ chars world) toShow where
		xNow = xFirst world
		yNow = yFirst world
		toShow =
			if action world == Pick
			then zip3 [0..] alphabet $ (\(_,_,a,b) -> (a,b)) <$>
				filter (\(x,y,_,_) -> x == xNow && y == yNow) (items world)
			else zipWith (\n (c,x) -> (n,c,x)) [0..] $ M.toList $ inv $ getFirst world 
		word = if isPick then "pick" else "drop"
-- | draw parts with bindings in equipment menu
drawPartChange :: World -> Int -> IO ()
drawPartChange world _ = do
	wAttrSet stdScr (attr0, Pair defaultc)
	mvWAddStr stdScr 0 0 msgHeaderBind
	mvWAddStr stdScr 0 shiftW "Weapon"
	mvWAddStr stdScr 0 shiftA "Armor"
	mvWAddStr stdScr 0 shiftJ "Jewelry"
	sequence_ $ zipWith3 ($) (drawPartFull True 1 <$> [1..])
		(repeat $ getFirst world) $ parts $ getFirst world
	wAttrSet stdScr (setBold attr0 True, Pair defaultc)
	mvAddCh (shift world + 1) (diff * fromEnum (slot world) + shiftW - 1) $ castEnum '>'

addDataToShow :: DataToShow -> M.Map (Int, Int) DataToShow -> M.Map (Int, Int) DataToShow
addDataToShow Nothing m = m
addDataToShow dts@(Just(x, y, ch, attr, fore, _)) m = 
	case M.lookup (x, y) m of
		Nothing -> M.insert (x, y) dts m
		Just Nothing -> putWE "addDataToShow"
		Just (Just (_, _, _, attr', _, back)) ->
			M.insert (x, y) (Just (x, y, ch, attrPlus attr attr', fore, back)) m

-- | draw just a world after another step
drawJustWorld :: World -> Int -> IO ()
drawJustWorld world _ = do
	wAttrSet stdScr (attr0, Pair defaultc)
	(_, w) <- scrSize
	showMessages w $ message world

	--mapM_ (drawCell world) $ A.assocs $ worldmap world
	--mapM_ (drawItem world) $ items world
	--mapM_ (drawUnit world) $ M.toList $ units world
	let todo = foldr addDataToShow M.empty $ 
		map (dataToShowUnit world) (M.toList $ units world) ++
		map (dataToShowItem world) (items world) ++
		map (dataToShowCell world) (A.assocs $ worldmap world)
	mapM_ showByData $ M.elems todo
	sequence_ $ zipWith3 ($) (drawPart False <$> [0..])
		(repeat mon) $ sortBy (on compare kind) $ parts mon
	wAttrSet stdScr (attr0, Pair defaultc)
	mvWAddStr stdScr shiftDown shiftAttrs $ "Slowness: " ++ 
		show (effectiveSlowness mon)
	mvWAddStr stdScr (shiftDown + 1) shiftAttrs $ "XP: " ++ 
		show (xp mon) ++ "; Level: " ++ show (intLog $ xp mon)
	wAttrSet stdScr (attr0, Pair yellow)
	unless (encumbrance mon <= capacity mon)
		$ mvWAddStr stdScr (shiftDown + 2) shiftAttrs "Burdened"
	wAttrSet stdScr (attr0, Pair defaultc)
	mvWAddStr stdScr (shiftDown + 3) shiftAttrs
			$ "Next wave: " ++ show (wave world)
	mapM_ (showElemRes world) (getAll :: [Elem])
	mapM_ (showIntr world) (getAll :: [Intr])
	mapM_ (showTemp world) (getAll :: [Temp]) where
		mon = getFirst world
-- | draw list of all recipes
drawCraft :: World -> Int -> IO ()
drawCraft w _ = wAttrSet stdScr (attr0, Pair defaultc) >>
	mvWAddStr stdScr 0 0 msgHeaderCraft >>
	zipWithM_ (showRecipe $ inv $ getFirst w) recipes [1..]
-- | draw split menu
drawSplit :: World -> Int -> IO ()
drawSplit w _ = wAttrSet stdScr (attr0, Pair defaultc) >>
	mvWAddStr stdScr 0 0 (show $ numToSplit w) >>
	drawJustWorld w (putWE "drawJustWorld")
-- | draw options menu
drawOptions :: World -> Int -> IO ()
drawOptions _ _ = do
	wAttrSet stdScr (attr0, Pair defaultc)
	mvWAddStr stdScr 0 0 msgOptColorHei
	mvWAddStr stdScr 1 0 msgOptColorMon
	mvWAddStr stdScr 2 0 msgOptNoHei
	mvWAddStr stdScr 3 0 msgOptColorHeiAbs
-- | choose a draw mode and draw all
draw :: World -> IO ()
draw world = do
	(h, _) <- scrSize
	(case action world of 
		Inventory -> drawInventory
		Bind -> drawEquipMenu
		Pick -> drawPickOrDrop True
		DropMany -> drawPickOrDrop False
		Equip -> drawPartChange
		Split2 -> drawSplit
		Craft -> drawCraft
		Options -> drawOptions
		_ -> drawJustWorld) world h
-- | redraw world and get char from the player
redraw :: World -> IO Char
redraw world = 
	erase >> draw world >> refresh >> getChar
-- | show info about one body part (with bindings)
drawPart :: Bool -> Int -> Monster -> Part -> IO ()
drawPart isFull y = drawPartFull isFull shiftRightHP $ shiftDown + y
-- | show info about a part (with bindings iff first argument is True)
drawPartFull :: Bool -> Int -> Int -> Monster -> Part -> IO ()
drawPartFull isFull x y mon part = do
	wAttrSet stdScr (attr0, Pair green)
	when (hp part * 3 < maxhp part * 2 || hp part <= 10)
		$ wAttrSet stdScr (attr0, Pair yellow)
	when (hp part * 3 < maxhp part || hp part <= 5)
		$ wAttrSet stdScr (attr0, Pair red)
	when (hp part * 8 < maxhp part || hp part <= 3)
		$ wAttrSet stdScr (attr0, Pair redInverse)
	mvWAddStr stdScr y x     str1
	mvWAddStr stdScr y (x+5) str2
	mvWAddStr stdScr y (x+shiftW-1) strW
	mvWAddStr stdScr y (x+shiftA-1) strA
	mvWAddStr stdScr y (x+shiftJ-1) strJ
	where
		str1 = show (kind part) ++ ":"
		str2 = show (hp part) ++ "/" ++ show (maxhp part) ++
			" rr: " ++ show (regRate part)
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

-- | symbol to show item on the ground
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
symbolItem (Food {})     = '%'
symbolItem (Resource {}) = '0'
symbolItem (Tool {})     = '\\'
