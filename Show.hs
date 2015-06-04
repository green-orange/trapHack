module Show where

import Data
import Utils4all

import UI.HSCurses.Curses
import Data.Set (empty, member, Set)

shiftDown = 5 :: Int
shiftRightHP1 = 25 :: Int
shiftRightHP2 = 30 :: Int

dEFAULT   = 1 :: Int
sAFE      = 2 :: Int
uNSAFE    = 3 :: Int
dANGER    = 4 :: Int
nEARDEATH = 5 :: Int

castEnum = toEnum . fromEnum

drawUnit :: Unit -> IO ()
drawUnit (x, y, mon) = mvAddCh (y + shiftDown) x $ castEnum $ symbolMon $ name mon

drawCell :: (Int, Int, Terrain) -> IO ()
drawCell (x, y, t) = mvAddCh (y + shiftDown) x $ castEnum $ symbolTer t

drawItem :: (Int, Int, Object, Int) -> IO ()
drawItem (x, y, item, _) = mvAddCh (y + shiftDown) x $ castEnum $ symbolItem item

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
			stringsToShow = zip [1..] $ map (\(c, obj, n) -> 
				[c] ++ " - " ++ (show n) ++ " * " ++ titleShow obj) items
			showInv :: (Int, String) -> IO ()
			showInv (n, s) = mvWAddStr stdScr n 0 $ s
			in do
			(attr, pair) <- wAttrGet stdScr
			wAttrSet stdScr (attr, Pair dEFAULT)
			mvWAddStr stdScr 0 0 "Your inventory: (press Enter or Space to close it)"
			foldl (>>) doNothing $ map showInv stringsToShow
		',' -> let
			(xNow, yNow, _) = head $ units world
			toShow = zip3 alphabet [1..] $ map (\(_,_,a,b) -> (a,b)) $
				filter (\(x,y,_,_) -> x == xNow && y == yNow) $ items world
			in do
			(attr, pair) <- wAttrGet stdScr
			wAttrSet stdScr (attr, Pair dEFAULT)
			mvWAddStr stdScr 0 0 "What do you want to pick up? (press Enter to finish)"
			foldl (>>) doNothing $ map (showItemsOnGround $ toPick world) toShow
		_ -> do
			(attr, pair) <- wAttrGet stdScr
			wAttrSet stdScr (attr, Pair dEFAULT)
			mvWAddStr stdScr 0 0 $ message world
			foldl (>>) doNothing $ map drawCell $ flatarray2line $ worldmap world
			foldl (>>) doNothing $ map drawItem $ items world
			foldl (>>) doNothing $ map drawUnit $ units world
			mvWAddStr stdScr 30 30 $ show $ action world : store world -- debug
			mvWAddStr stdScr 40 40 $ show $ stdgen world -- debug
			foldl (>>) doNothing $ zipWith ($) (map drawPart $ parts $ getFirst world) [1..]

drawPart :: Part -> Int -> IO ()
drawPart part x = do
	(attr, pair) <- wAttrGet stdScr
	wAttrSet stdScr (attr, Pair sAFE)
	if hp part * 2 < maxhp part
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
symbolMon _            = error "wrong monster"

symbolTer :: Terrain -> Char
symbolTer t
	| t == eMPTY    = '.'
	| t == bEARTRAP = '#'
	| otherwise     = error "wrong terrain"

symbolItem :: Object -> Char
symbolItem (Potion _ _)   = '!'
symbolItem (Wand _ _ _ _) = '/'
symbolItem (Trap _ _)     = '^'
symbolItem _              = error "wrong object"

