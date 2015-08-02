module IO.Colors where

import Data.Define

import UI.HSCurses.Curses
import Data.Maybe (catMaybes)
import Data.Functor ((<$>))

-- | constants with handles of foreground colors
dEFAULT, gREEN, yELLOW, rED, cYAN, mAGENTA, bLUE, rEDiNVERSE :: Int
dEFAULT    = 1
rED        = 2
gREEN      = 3
yELLOW     = 4
bLUE       = 5
mAGENTA    = 6
cYAN       = 7
rEDiNVERSE = 58

-- | converts terrain to handle of a background color
colorFromTerr :: Terrain -> Int
colorFromTerr Empty        = 8  -- default
colorFromTerr BearTrap     = 32 -- yellow
colorFromTerr FireTrap     = 16 -- red
colorFromTerr PoisonTrap   = 56 -- green
colorFromTerr MagicTrap    = 48 -- magenta
colorFromTerr Water        = 40 -- blue
colorFromTerr Bonfire      = 16 -- red
colorFromTerr MagicNatural = 48 -- magenta

-- | converts cell to handle of a background color
colorFromCell :: Cell -> Int
colorFromCell = colorFromTerr . terrain

-- | converts height difference to a color
colorFromHei :: Int -> Int
colorFromHei hei
	| hei  < -3 = dEFAULT
	| hei  < -1 = rED
	| hei == -1 = yELLOW
	| hei ==  0 = gREEN
	| hei ==  1 = cYAN
	| hei  <  4 = bLUE
	| otherwise = mAGENTA

-- | converts absolute height to a color
colorFromHeiAbs :: Int -> Int
colorFromHeiAbs hei
	| hei  <  2 = rED
	| hei  <  4 = yELLOW
	| hei ==  4 = gREEN
	| hei ==  5 = cYAN
	| hei  <  7 = bLUE
	| otherwise = mAGENTA

-- | convert a temporary effect to a color
colorFromTemp :: Temp -> Int -> Int
colorFromTemp Nutrition n
	| n <= 5  = rEDiNVERSE
	| n <= 20 = rED
	| n <= 50 = yELLOW
	| otherwise = dEFAULT
colorFromTemp Poison _ = rED
colorFromTemp Conf _ = yELLOW
colorFromTemp Stun _ = rED

-- | initialize handle of all color pairs
initColors :: IO ()
initColors = sequence_ actions where
	colorList = ["red", "green", "yellow", "blue", "magenta", "cyan", "white"]
	colorListFore = defaultForeground : catMaybes (color <$> colorList)
	colorListBack = defaultBackground : catMaybes (color <$> colorList)
	bindColor n = initPair (Pair n) (colorListFore !! mod (n-1) 8) (colorListBack !! div (n-1) 8)
	actions = bindColor <$> [1..64]

-- | char and color by a monster name
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
symbolMon "Tree"              = ('Y', gREEN)
symbolMon "Bot"               = ('b', cYAN)
symbolMon "Bee"               = ('b', yELLOW)
symbolMon _                   = error "unknown monster"
