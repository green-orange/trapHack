module Main where

import Data
import Monsters (getPlayer)
import Step
import Changes (clearMessage)
import Show
import Utils4all

import UI.HSCurses.Curses
import System.Random (StdGen(..), getStdGen)
import Data.Set (empty)
import System.Posix.User

initWorld :: String -> StdGen -> World
initWorld username gen = World {
	worldmap = [[0 | y <- [0..maxY]] | x <- [0..maxX]],
	dirs = rectdirs (0, 0, maxX, maxY),
	units = [(div maxX 2, div maxY 2, getPlayer (div maxX 2) (div maxY 2))],
	message = [("Welcome to the TrapHack, " ++ username ++ ".", bLUE)],
	items = [],
	action = ' ',
	stdgen = gen,
	wave = 1,
	toPick = empty,
	prevAction = ' ',
	stepsBeforeWave = 1
}

loop :: World -> IO String
loop world =
	if isPlayerNow world
	then do
		erase
		draw world
		refresh
		c <- getCh
		if (c == KeyChar 'Q')
		then
			if wave world == 1
			then return "You quit. Do not pass go. Do not collect 200 zorkmids."
			else return $ "You quit on the " ++ numToStr (wave world - 1) ++ " wave."
		else
			case (step (clearMessage world) c) of
				Nothing -> return $ "You died on the " ++ numToStr (wave world - 1) ++ " wave."
				Just newMWorld -> loop newMWorld
	else
		case step world $ KeyChar ' ' of
			Nothing -> do
				erase
				draw world
				refresh
				getCh
				return $ "You died on the " ++ numToStr (wave world - 1) ++ " wave."
			Just newMWorld -> loop newMWorld

main :: IO ()
main = do
	initScr
	(h, w) <- scrSize
	if (w <= 40 || h <= 25)
	then do
		putStrLn "Your screen is too small"
		endWin
	else do
		gen <- getStdGen
		username <- getLoginName
		initCurses
		startColor
		initColors
		keypad stdScr True
		echo False
		cursSet CursorInvisible
		msg <- loop $ initWorld username gen
		endWin
		putStrLn msg
