{-# LANGUAGE CPP #-}
module Main where

import Data.Const
import Data.World
import Data.Define
import Data.Monster (units)
import Utils.Changes (clearMessage)
import Utils.Monsters (intLog)
import IO.Step
import IO.Show
import IO.Colors
import IO.Texts
import IO.Read (separate)
import Init

import UI.HSCurses.Curses
import Control.Monad (unless, liftM)
import System.Random (getStdGen)
import Control.Exception (catch, SomeException)
import Data.Time.Clock
import qualified Data.Map as M
#ifndef mingw32_HOST_OS
import System.Posix.User
#endif

logName, saveName :: String
logName = "trapHack.log"
saveName = "trapHack.save"

catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll = catch

getReverseLog :: IO [(String, Int)]
getReverseLog = liftM (map (flip (,) dEFAULT) . tail . reverse 
	. separate '\n') $ readFile logName

playerLevel :: World -> Int
playerLevel w = intLog $ xp player where
	player = snd . head $ M.toList $ M.filter (("You" ==) . name) $ units w

loop :: World -> IO (String, Int)
loop world =
	if isPlayerNow world
	then do
		c <- redraw world
		(_, width) <- scrSize
		case step (clearMessage width world) c of
			Left newWorld -> case action newWorld of
				Save -> do
					writeFile saveName $ show newWorld
					return (msgSaved, playerLevel world)
				Previous -> do
					msgs <- getReverseLog
					loop newWorld {action = AfterSpace, message = msgs}
				AfterSpace -> loop newWorld
				_ -> do
					maybeAppendFile logName $ filter (not . null) 
						$ map fst $ message world
					loop newWorld
			Right msg ->
				writeFile saveName "" >> appendFile logName (msg ++ "\n")
				>> return (msg, playerLevel world)
	else
		case step world ' ' of
			Left newWorld -> loop newWorld
			Right msg -> redraw world >> 
				appendFile logName (msg ++ "\n") >>
				return (msg, playerLevel world)
	where
	maybeAppendFile fileName strings = 
		unless (null strings) $ appendFile fileName $ unwords strings ++ "\n"

main :: IO ()
main = do
	save <- catchAll (readFile saveName) $ const $ return ""
	unless (null save) $ print msgAskLoad
	ans <- if null save then return 'n' else getChar
	_ <- initScr
	(h, w) <- scrSize
	_ <- endWin
	if w <= 2 * xSight + 42 || h <= 2 * ySight + 5
	then putStrLn msgSmallScr
	else do gen <- getStdGen
#ifndef mingw32_HOST_OS
		username <- getLoginName
#else
		print msgAskName
		username <- getLine
#endif
		timeBegin <- getCurrentTime
		maybeWorld <-
			if ans == 'y' || ans == 'Y'
			then catchAll (return $ Just $ read save) $ const $ return Nothing
			else do	
				writeFile logName ""
				return $ Just $ initWorld username gen
		case maybeWorld of
			Nothing -> endWin >> putStrLn msgLoadErr
			Just world ->
				initScr >> initCurses >> startColor >> initColors >>
				keypad stdScr True >> echo False >>
				cursSet CursorInvisible >> 
				catchAll (do
					(msg, lvl) <- loop world 
					endWin
					putStrLn msg
					timeEnd <- getCurrentTime
					putStr "Time in game: "
					print $ diffUTCTime timeEnd timeBegin
					putStr "Level: " 
					print lvl)
				(\e -> endWin >> putStrLn (msgGameErr ++ show e))
