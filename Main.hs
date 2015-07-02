{-# LANGUAGE CPP #-}
module Main where

import Data
import Step
import Changes (clearMessage)
import Show
import Init
import Colors

import UI.HSCurses.Curses
import System.Random (getStdGen)
#if linux_HOST_OS
import System.Posix.User
#endif

loop :: World -> IO String
loop world =
	if isPlayerNow world
	then do
		c <- redraw world
		case step (clearMessage world) c of
			Left newWorld -> loop newWorld
			Right msg -> return msg
	else
		case step world $ KeyChar ' ' of
			Left newWorld -> loop newWorld
			Right msg -> redraw world >> return msg

main :: IO ()
main = do
	_ <- initScr
	(h, w) <- scrSize
	_ <- endWin
	if (w <= 2 * xSight + 42 || h <= 2 * ySight + 5)
	then putStrLn "Your screen is too small"
	else do gen <- getStdGen
#if linux_HOST_OS
		username <- getLoginName
#else
		print "What's your name?"
		username <- getLine
#endif
		initScr >> initCurses >> startColor >> initColors >>
			keypad stdScr True >> echo False >>
			cursSet CursorInvisible >> return ()
		(loop $ initWorld username gen) >>= (\msg -> endWin >> putStrLn msg)
