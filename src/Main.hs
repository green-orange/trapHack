{-# LANGUAGE CPP #-}
{-|
Module      : Main
Description : roguelike game with puzzle elements
Copyright   : (c) Khadaev Konstantin, 2015
License     : Unlicense
Maintainer  : khadaev98@gmail.com
Stability   : experimental
Portability : POSIX
-}
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
import System.Time.Utils (renderSecs)
import Data.Functor ((<$>))
import qualified Data.Map as M
#ifndef mingw32_HOST_OS
import System.Posix.User
#endif

logName, saveName, resName :: String
-- | file with the game log
logName = "traphack.log"
-- | file with the game save
saveName = "traphack.save"
-- | file with results
resName = "traphack.res"

-- | catch all exceptions to run 'endWin' after exit with error
catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll = catch

-- | read file with name 'logName' and adapt it to show as in-game message
getReverseLog :: IO [(String, Int)]
getReverseLog = liftM (map (flip (,) defaultc) . tail . reverse 
	. separate '\n') $ readFile logName

-- | gives XP level of the player
playerLevel :: World -> Int
playerLevel w = intLog $ xp player where
	player = snd . head $ M.toList $ M.filter (("You" ==) . name) $ units w

-- | main loop in the game
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
						$ fst <$> message world
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

-- | choose all parameters and start or load the game
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
		maybeWorld <-
			if ans == 'y' || ans == 'Y'
			then catchAll (return $ Just $ read save) $ const $ return Nothing
			else do
				mapgen <- showMapChoice
				char <- showCharChoice
				writeFile logName ""
				return $ Just $ initWorld mapgen char username gen
		timeBegin <- getCurrentTime
		case maybeWorld of
			Nothing -> endWin >> putStrLn msgLoadErr
			Just world ->
				initScr >> initCurses >> startColor >> initColors >>
				keypad stdScr True >> echo False >>
				cursSet CursorInvisible >> 
				catchAll (do
					(msg, lvl) <- loop world 
					endWin
					timeEnd <- getCurrentTime
					let str = msg ++ "\nTime in game: " ++
						renderSecs (round $ diffUTCTime timeEnd timeBegin) ++
						"\nLevel: " ++ show lvl ++ "\n\n"
					putStr str
					appendFile resName str
					)
				(\e -> endWin >> putStrLn (msgGameErr ++ show e))
