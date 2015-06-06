module Show where

import Data
import Utils4all

import UI.HSCurses.Curses
import Data.Set (empty, member, Set)

shiftDown = 5 :: Int
shiftRightHP1 = 25 :: Int
shiftRightHP2 = 30 :: Int

dEFAULT   = tRAPSNUM + 1
sAFE      = tRAPSNUM + 2
uNSAFE    = tRAPSNUM + 3
dANGER    = tRAPSNUM + 4
nEARDEATH = tRAPSNUM + 5

castEnum = toEnum . fromEnum

drawUnit :: World -> Unit -> IO ()
drawUnit world (x, y, mon) = do
	(attr, _) <- wAttrGet stdScr
	wAttrSet stdScr (attr, Pair $ worldmap world !! x !! y)
	mvAddCh (y + shiftDown) x $ castEnum $ symbolMon $ name mon

drawCell :: World -> (Int, Int, Terrain) -> IO ()
drawCell world (x, y, t) = do
	(attr, _) <- wAttrGet stdScr
	wAttrSet stdScr (attr, Pair $ worldmap world !! x !! y)
	mvAddCh (y + shiftDown) x $ castEnum $ symbolTer t

drawItem :: World -> (Int, Int, Object, Int) -> IO ()
drawItem world(x, y, item, _) = do
	(attr, _) <- wAttrGet stdScr
	wAttrSet stdScr (attr, Pair $ worldmap world !! x !! y)
	mvAddCh (y + shiftDown) x $ castEnum $ symbolItem item

showItemsOnGround :: (Set Char) -> (Char, Int, (Object, Int)) -> IO ()
showItemsOnGround toPick (c, n, (obj,cnt)) =
	mvWAddStr stdScr (n + 1) 0 ([c] ++ sym ++ (show cnt) ++ " * " ++ titleShow obj) where
	sym =
		if member c toPick
		then " + "
		else " - "

draw :: World -> IO()
draw world =
	case action world of
		'i' -> let
			items = inv $ getFirst world
			wield :: Char -> String
			wield c = 
				if (weapon $ getFirst world) == c
				then " (wielded)"
				else "" 
			stringsToShow = zip [1..] $ map (\(c, obj, n) -> 
				[c] ++ " - " ++ (show n) ++ " * " ++ titleShow obj ++ wield c) items
			showInv :: (Int, String) -> IO ()
			showInv (n, s) = mvWAddStr stdScr n 0 $ s
			in do
			(attr, _) <- wAttrGet stdScr
			wAttrSet stdScr (attr, Pair dEFAULT)
			mvWAddStr stdScr 0 0 "Your inventory: (press Enter or Space to close it)"
			foldl (>>) doNothing $ map showInv stringsToShow
		',' -> let
			(xNow, yNow, _) = head $ units world
			toShow = zip3 alphabet [1..] $ map (\(_,_,a,b) -> (a,b)) $
				filter (\(x,y,_,_) -> x == xNow && y == yNow) $ items world
			in do
			(attr, _) <- wAttrGet stdScr
			wAttrSet stdScr (attr, Pair dEFAULT)
			mvWAddStr stdScr 0 0 "What do you want to pick up? (press Enter to finish)"
			foldl (>>) doNothing $ map (showItemsOnGround $ toPick world) toShow
		_ -> do
			(attr, _) <- wAttrGet stdScr
			wAttrSet stdScr (attr, Pair dEFAULT)
			mvWAddStr stdScr 0 0 $ message world
			foldl (>>) doNothing $ map (drawCell world) $ flatarray2line $ worldmap world
			foldl (>>) doNothing $ map (drawItem world) $ items world
			foldl (>>) doNothing $ map (drawUnit world) $ units world
			{-
			mvWAddStr stdScr 30 30 $ show $ action world : store world -- debug
			mvWAddStr stdScr 40 40 $ show $ stdgen world -- debug
			-}
			foldl (>>) doNothing $ zipWith ($) (map drawPart $ parts $ getFirst world) [1..]

drawPart :: Part -> Int -> IO ()
drawPart part x = do
	(attr, pair) <- wAttrGet stdScr
	wAttrSet stdScr (attr, Pair sAFE)
	if hp part * 3 < maxhp part * 2
	then wAttrSet stdScr (attr, Pair uNSAFE)
	else doNothing
	if hp part * 3 < maxhp part
	then wAttrSet stdScr (attr, Pair dANGER)
	else doNothing
	if hp part * 8 < maxhp part
	then wAttrSet stdScr (attr, Pair nEARDEATH)
	else doNothing
	mvWAddStr stdScr (shiftDown + x) shiftRightHP1 str1
	mvWAddStr stdScr (shiftDown + x) shiftRightHP2 str2
	where
		str1 = (partToStr $ kind part) ++ ":"
		str2 = (show $ hp part) ++ "/" ++ (show $ maxhp part)

flatarray2line' :: Int -> [[a]] -> [(Int, Int, a)]
flatarray2line' _ [] = []
flatarray2line' start (x:xs) = (zip3 [start, start..] [0, 1..] x) ++ (flatarray2line' (start + 1) xs)

flatarray2line = flatarray2line' 0

symbolMon :: String -> Char
symbolMon "You"        = '@'
symbolMon "Homunculus" = 'h'
symbolMon "Beetle"     = 'a'
symbolMon "Bat"        = 'B'
symbolMon "Hunter"     = 'H'
symbolMon _            = error "unknown monster"

symbolTer :: Terrain -> Char
symbolTer t
	| t == eMPTY    = '.'
	| otherwise     = '^'

symbolItem :: Object -> Char
symbolItem (Potion _ _)     = '!'
symbolItem (Wand _ _ _ _)   = '/'
symbolItem (Trap _ _)       = '^'
symbolItem (Missile _ _ _)  = ']'
symbolItem (Weapon _ _)     = ')'
symbolItem (Launcher _ _ _) = '}'
symbolItem _              = error "unknown object"

