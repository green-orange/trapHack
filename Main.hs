{-# LANGUAGE CPP #-}
module Main where

import Data
import Step
import Changes (clearMessage)
import Show
import Init

import UI.HSCurses.Curses
import System.Random (StdGen(..), getStdGen)
import Data.Set (empty)
import Data.Map (singleton)
#if linux_HOST_OS
import System.Posix.User
#endif

loop :: World -> IO String
loop world =
	if isPlayerNow world
	then do
		erase
		draw world
		refresh
		c <- getCh
		case step (clearMessage world) c of
			Left newWorld -> loop newWorld
			Right msg -> return msg
	else
		case step world $ KeyChar ' ' of
			Left newWorld -> loop newWorld
			Right msg -> do
				erase
				draw world
				refresh
				getCh
				return msg

main :: IO ()
main = do
	initScr
	(h, w) <- scrSize
	if (w <= maxX + 20 || h <= maxY + 10)
	then do
		putStrLn "Your screen is too small"
		endWin
	else do
		endWin
		gen <- getStdGen
#if linux_HOST_OS
		username <- getLoginName
#else
		print "What's your name?"
		username <- getLine
#endif
		initScr
		initCurses
		startColor
		initColors
		keypad stdScr True
		echo False
		cursSet CursorInvisible
		msg <- loop $ initWorld username gen
		endWin
		putStrLn msg
