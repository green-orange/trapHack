module IO.Colors where

import Data.Define

import UI.HSCurses.Curses
import Data.Maybe (fromJust)

dEFAULT, gREEN, yELLOW, rED, cYAN, mAGENTA, bLUE, rEDiNVERSE :: Int
dEFAULT    = 1
rED        = 2
gREEN      = 3
yELLOW     = 4
bLUE       = 5
mAGENTA    = 6
cYAN       = 7
rEDiNVERSE = 58

colorFromTerr :: Terrain -> Int
colorFromTerr Empty      = 8  -- default
colorFromTerr BearTrap   = 32 -- yellow
colorFromTerr FireTrap   = 16 -- red
colorFromTerr PoisonTrap = 56 -- green
colorFromTerr MagicTrap  = 48 -- magenta

colorFromCell :: Cell -> Int
colorFromCell = colorFromTerr . terrain

colorFromTemp :: Temp -> Int -> Int
colorFromTemp Nutrition n
	| n <= 5  = rEDiNVERSE
	| n <= 20 = rED
	| n <= 50 = yELLOW
	| otherwise = dEFAULT
colorFromTemp Poison _ = rED
colorFromTemp Conf _ = yELLOW
colorFromTemp Stun _ = rED

initColors :: IO ()
initColors = sequence_ actions where
	colorList = ["red", "green", "yellow", "blue", "magenta", "cyan", "white"]
	colorListFore = defaultForeground : map (fromJust . color) colorList
	colorListBack = defaultBackground : map (fromJust . color) colorList
	bindColor n = initPair (Pair n) (colorListFore !! mod (n-1) 8) (colorListBack !! div (n-1) 8)
	actions = map bindColor [1..64]

symbolMon :: String -> (Char, Int)
symbolMon "You"               = ('@', yELLOW)
symbolMon "Homunculus"        = ('h', yELLOW)
symbolMon "Beetle"            = ('a', cYAN)
symbolMon "Bat"               = ('B', rED)
symbolMon "Hunter"            = ('H', yELLOW)
symbolMon "Ivy"               = ('I', gREEN)
symbolMon "Dummy"             = ('&', bLUE)
symbolMon "Garbage collector" = ('G', bLUE)
symbolMon "Accelerator"       = ('A', yELLOW)
symbolMon "Troll"             = ('T', rED)
symbolMon "Rock"              = ('#', bLUE)
symbolMon "Tail"              = ('~', bLUE)
symbolMon "Worm"              = ('w', rED)
symbolMon "Golem"             = ('g', bLUE)
symbolMon "Floating eye"      = ('e', mAGENTA)
symbolMon "Red dragon"        = ('D', rED)
symbolMon "White dragon"      = ('D', dEFAULT)
symbolMon "Green dragon"      = ('D', gREEN)
symbolMon "Forgotten beast"   = ('X', mAGENTA)
symbolMon "Spider"            = ('s', rED)
symbolMon "Soldier"           = ('@', yELLOW)
symbolMon "Umber hulk"        = ('U', yELLOW)
symbolMon "Tree"              = ('7', gREEN)
symbolMon _                   = error "unknown monster"
