module IO.Colors where

import Data.Define
import Data.ID

import UI.HSCurses.Curses
import Data.Maybe (catMaybes)
import Data.Functor ((<$>))

-- | constants with handles of foreground colors
backMult, defaultc, defaultBack, red, green, yellow, blue, magenta, cyan, redInverse :: Int
backMult    = 8
defaultc    = 1
defaultBack = defaultc * backMult
red         = 2
green       = 3
yellow      = 4
blue        = 5
magenta     = 6
cyan        = 7
redInverse  = backMult * (backMult - 1) + red

-- | converts terrain to handle of a background color
colorFromTerr :: Terrain -> Int
colorFromTerr Empty        = defaultBack
colorFromTerr BearTrap     = yellow  * backMult
colorFromTerr FireTrap     = red     * backMult
colorFromTerr PoisonTrap   = green   * backMult
colorFromTerr MagicTrap    = magenta * backMult
colorFromTerr Water        = blue    * backMult
colorFromTerr Bonfire      = red     * backMult
colorFromTerr MagicNatural = magenta * backMult

-- | converts cell to handle of a background color
colorFromCell :: Cell -> Int
colorFromCell = colorFromTerr . terrain

-- | converts height difference to a color
colorFromHei :: Int -> Int
colorFromHei hei
	| hei  < -3 = defaultc
	| hei  < -1 = red
	| hei == -1 = yellow
	| hei ==  0 = green
	| hei ==  1 = cyan
	| hei  <  4 = blue
	| otherwise = magenta

-- | converts absolute height to a color
colorFromHeiAbs :: Int -> Int
colorFromHeiAbs hei
	| hei  <  2 = red
	| hei  <  4 = yellow
	| hei ==  4 = green
	| hei ==  5 = cyan
	| hei  <  7 = blue
	| otherwise = magenta

-- | convert a temporary effect to a color
colorFromTemp :: Temp -> Int -> Int
colorFromTemp Nutrition n
	| n <= 5  = redInverse
	| n <= 20 = red
	| n <= 50 = yellow
	| otherwise = defaultc
colorFromTemp Poison _ = red
colorFromTemp Conf _ = yellow
colorFromTemp Stun _ = red

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
	| x == idYou = ('@', yellow)
	| x == idHom = ('h', yellow)
	| x == idBtl = ('a', cyan)
	| x == idBat = ('B', red)
	| x == idHun = ('H', yellow)
	| x == idIvy = ('I', green)
	| x == idDum = ('&', blue)
	| x == idGrC = ('G', blue)
	| x == idAcc = ('A', yellow)
	| x == idTrl = ('T', red)
	| x == idRck = ('#', blue)
	| x == idTai = ('~', blue)
	| x == idWrm = ('w', red)
	| x == idGlm = ('g', blue)
	| x == idFlE = ('e', magenta)
	| x == idRDr = ('D', red)
	| x == idWDr = ('D', defaultc)
	| x == idGDr = ('D', green)
	| x == idFgB = ('X', magenta)
	| x == idSpd = ('s', red)
	| x == idSol = ('@', yellow)
	| x == idUmH = ('U', yellow)
	| x == idTre = ('Y', green)
	| x == idBot = ('$', cyan)
	| x == idBee = ('b', yellow)
	| x == idBsh = ('y', green)
	| otherwise = error "unknown monster"
