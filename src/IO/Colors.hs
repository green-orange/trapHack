module IO.Colors where

import Data.Define
import Data.ID

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
symbolMon :: Int -> (Char, Int)
symbolMon x
	| x == idYou = ('@', yELLOW)
	| x == idHom = ('h', yELLOW)
	| x == idBtl = ('a', cYAN)
	| x == idBat = ('B', rED)
	| x == idHun = ('H', yELLOW)
	| x == idIvy = ('I', gREEN)
	| x == idDum = ('&', bLUE)
	| x == idGrC = ('G', bLUE)
	| x == idAcc = ('A', yELLOW)
	| x == idTrl = ('T', rED)
	| x == idRck = ('#', bLUE)
	| x == idTai = ('~', bLUE)
	| x == idWrm = ('w', rED)
	| x == idGlm = ('g', bLUE)
	| x == idFlE = ('e', mAGENTA)
	| x == idRDr = ('D', rED)
	| x == idWDr = ('D', dEFAULT)
	| x == idGDr = ('D', gREEN)
	| x == idFgB = ('X', mAGENTA)
	| x == idSpd = ('s', rED)
	| x == idSol = ('@', yELLOW)
	| x == idUmH = ('U', yELLOW)
	| x == idTre = ('Y', gREEN)
	| x == idBot = ('$', cYAN)
	| x == idBee = ('b', yELLOW)
	| x == idBsh = ('y', gREEN)
	| otherwise = error "unknown monster"
