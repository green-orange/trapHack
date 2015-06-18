module Show where

import Data
import Parts
import Messages

import UI.HSCurses.Curses
import Data.Set (member, Set)
import Data.Maybe
import Data.Map (toList)
import Data.List (sortBy)
import Data.Function (on)

shiftDown, shiftRightHP1, shiftRightHP2, shiftAttrs :: Int
shiftDown = 5
shiftRightHP1 = maxX + 5
shiftRightHP2 = maxX + 10
shiftAttrs = maxX + 20

castEnum :: Char -> ChType
castEnum = toEnum . fromEnum

initColors :: IO ()
initColors = do
	initPair (Pair eMPTY)      (defaultForeground)          (defaultBackground)
	initPair (Pair bEARTRAP)   (defaultForeground)          (fromJust $ color "yellow")
	initPair (Pair fIRETRAP)   (defaultForeground)          (fromJust $ color "red")
	initPair (Pair dEFAULT)    (defaultForeground)          (defaultBackground)
	initPair (Pair gREEN)      (fromJust $ color "green")   (defaultBackground)
	initPair (Pair yELLOW)     (fromJust $ color "yellow")  (defaultBackground)
	initPair (Pair rED)        (fromJust $ color "red")     (defaultBackground)
	initPair (Pair rEDiNVERSE) (fromJust $ color "red")     (fromJust $ color "white")
	initPair (Pair cYAN)       (fromJust $ color "cyan")    (defaultBackground)
	initPair (Pair mAGENTA)    (fromJust $ color "magenta") (defaultBackground)
	initPair (Pair bLUE)       (fromJust $ color "blue")    (defaultBackground)

drawUnit :: World -> ((Int, Int), Monster) -> IO ()
drawUnit world ((x, y), mon) = do
	wAttrSet stdScr (attr, Pair $ worldmap world !! x !! y)
	mvAddCh (y + shiftDown) x $ castEnum $ symbolMon $ name mon where
		attr = 
			if x == xFirst world && y == yFirst world
			then setStandout attr0 True
			else attr0

drawCell :: World -> (Int, Int, Terrain) -> IO ()
drawCell world (x, y, _) = do
	wAttrSet stdScr (attr0, Pair $ worldmap world !! x !! y)
	mvAddCh (y + shiftDown) x $ castEnum '.'

drawItem :: World -> (Int, Int, Object, Int) -> IO ()
drawItem world(x, y, item, _) = do
	wAttrSet stdScr (attr0, Pair $ worldmap world !! x !! y)
	mvAddCh (y + shiftDown) x $ castEnum $ symbolItem item

showItemsPD :: Int -> (Set Char) -> (Int, Char, (Object, Int)) -> IO ()
showItemsPD h toPick' (n, c, (obj,cnt)) =
	mvWAddStr stdScr (mod (n + 1) h) (30 * (div (n + 1) h))
		([c] ++ sym ++ (show cnt) ++ " * " ++ titleShow obj) where
	sym =
		if member c toPick'
		then " + "
		else " - "
		
showMessages :: [(String, Int)] -> IO ()
showMessages msgs = (foldl (>>=) (return (0, 0)) $ map showMessage msgs) >> return ()

showMessage :: (String, Int) -> (Int, Int) -> IO (Int, Int)
showMessage (msg, color') (x, y) = do
	(_, w) <- scrSize
	wAttrSet stdScr (attr0, Pair color')
	mvWAddStr stdScr x y msg
	return $ rez w where
		rez w = (dx, dy) where
			dy' = y + 1 + length msg
			dx = x + div dy' w
			dy = mod dy' w

draw :: World -> IO()
draw world = do
	(h, _) <- scrSize
	if action world == 'i'
	then let
		items' = toList $ inv $ getFirst world
		wield :: Char -> String
		wield c = 
			if (weapon $ getFirst world) == c
			then " (wielded)"
			else "" 
		stringsToShow = zip [1..] $ map (\(c, (obj, n)) -> 
			[c] ++ " - " ++ (show n) ++ " * " ++ titleShow obj ++ wield c) items'
		showInv :: (Int, String) -> IO ()
		showInv (n, s) = mvWAddStr stdScr ((+) 1 $ mod n $ h-1) (30 * (div n $ h-1)) s
		in do
			wAttrSet stdScr (attr0, Pair dEFAULT)
			mvWAddStr stdScr 0 0 "Your inventory: (press Enter or Space to close it)"
			foldl (>>) doNothing $ map showInv stringsToShow
	else if (action world == ',' || action world == 'D')
	then let
		xNow = xFirst world
		yNow = yFirst world
		toShow =
			if action world == ','
			then zip3 [0..] alphabet $ map (\(_,_,a,b) -> (a,b)) $
				filter (\(x,y,_,_) -> x == xNow && y == yNow) $ items world
			else zipWith (\n (c,x) -> (n,c,x)) [0..] $ toList $ inv $ getFirst world 
		word = if action world == ',' then "pick" else "drop"
		in do
			wAttrSet stdScr (attr0, Pair dEFAULT)
			mvWAddStr stdScr 0 0 $ "What do you want to " ++ word 
				++ " up? (press Enter to finish)"
			foldl (>>) doNothing $ map (showItemsPD h $ chars world) toShow
	else do
		wAttrSet stdScr (attr0, Pair dEFAULT)
		showMessages $ message world
		foldl (>>) doNothing $ map (drawCell world) $ flatarray2line $ worldmap world
		foldl (>>) doNothing $ map (drawItem world) $ items world
		foldl (>>) doNothing $ map (drawUnit world) $ toList $ units world
		foldl (>>) doNothing $ zipWith ($) (map drawPart 
			$ sortBy (on compare kind) $ parts $ getFirst world) [0..]
		wAttrSet stdScr (attr0, Pair dEFAULT)
		mvWAddStr stdScr shiftDown shiftAttrs $ "Slowness: " ++ 
			(show $ effectiveSlowness $ getFirst world)
		case poison $ getFirst world of
			Nothing -> doNothing
			Just n -> mvWAddStr stdScr (shiftDown + 1) shiftAttrs 
				$ "Poison (" ++ show n ++ ")"
				
redraw :: World -> IO Key
redraw world = 
	erase >> draw world >> refresh >> getCh

drawPart :: Part -> Int -> IO ()
drawPart part x = do
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
	mvWAddStr stdScr (shiftDown + x) shiftRightHP1 str1
	mvWAddStr stdScr (shiftDown + x) shiftRightHP2 str2
	where
		str1 = (partToStr $ kind part) ++ ":"
		str2 = (show $ hp part) ++ "/" ++ (show $ maxhp part)

flatarray2line' :: Int -> [[a]] -> [(Int, Int, a)]
flatarray2line' _ [] = []
flatarray2line' start (x:xs) = (zip3 [start, start..] [0, 1..] x) 
	++ (flatarray2line' (start + 1) xs)

flatarray2line :: [[a]] -> [(Int, Int, a)]
flatarray2line = flatarray2line' 0

symbolMon :: String -> Char
symbolMon "You"               = '@'
symbolMon "Homunculus"        = 'h'
symbolMon "Beetle"            = 'a'
symbolMon "Bat"               = 'B'
symbolMon "Hunter"            = 'H'
symbolMon "Ivy"               = 'I'
symbolMon "Dummy"             = '&'
symbolMon "Garbage collector" = 'G'
symbolMon "Accelerator"       = 'A'
symbolMon "Troll"             = 'T'
symbolMon "Rock"              = '#'
symbolMon "Tail"              = '~'
symbolMon "Worm"              = 'w'
symbolMon "Golem"             = 'g'
symbolMon "Floating eye"      = 'e'
symbolMon "Dragon"            = 'D'
symbolMon "Forgotten beast"   = 'X'
symbolMon "Spider"            = 's'
symbolMon _                   = error "unknown monster"

symbolItem :: Object -> Char
symbolItem (Potion _ _)     = '!'
symbolItem (Scroll _ _)     = '?'
symbolItem (Wand _ _ _ _)   = '/'
symbolItem (Trap _ _)       = '^'
symbolItem (Missile _ _ _)  = ']'
symbolItem (Weapon _ _)     = ')'
symbolItem (Launcher _ _ _) = '}'
symbolItem _              = error "unknown object"

