{-# LANGUAGE CPP #-}
{-|
Module      : Main
Description : roguelike game with puzzle elements
Copyright   : (c) Khadaev Konstantin, 2016
License     : Unlicense
Maintainer  : khadaev98@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Main where

import Data.Const
import Data.World
import Data.Define
import Utils.Changes (clearMessage)
import IO.Step
import IO.Show
import IO.Colors
import IO.Texts
import IO.SaveLoad
import Init

import UI.HSCurses.Curses
import Control.Monad (unless, liftM)
import System.Random (getStdGen)
import Control.Exception (catch, SomeException)
import Control.DeepSeq
import Data.Time.Clock
import System.Time.Utils (renderSecs)
import Data.Functor ((<$>))
import Data.Maybe (listToMaybe)
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

-- | split string to a list of strings by given separator
separate :: Char -> String -> [String]
separate _ [] = [""]
separate c s = takeWhile (c /=) s : 
	case dropWhile (c /=) s of
		[] -> []
		_ : rest -> separate c rest

-- | read file with name 'logName' and adapt it to show as in-game message
getReverseLog :: IO [(String, Int)]
getReverseLog = liftM (map (flip (,) defaultc) . tail . reverse 
	. separate '\n') $ readFile logName

-- | main loop in the game
loop :: World -> IO (Exit, Bool)
loop world =
	if isPlayerNow world
	then do
		c <- redraw world
		(_, width) <- scrSize
		case step (clearMessage width world) c of
			Left newWorld -> case action newWorld of
				Save -> do
					writeFile saveName $ show $ saveWorld newWorld
					return (ExitSave, cheater world)
				Previous -> do
					msgs <- getReverseLog
					loop newWorld {action = AfterSpace, message = msgs}
				AfterSpace -> loop newWorld
				_ -> do
					maybeAppendFile logName $ filter (not . null) 
						$ fst <$> message world
					loop newWorld
			Right (exit, isCheater) ->
				writeFile saveName "" >> appendFile logName (msgByExit exit ++ "\n")
					>> return (exit, isCheater)
	else
		case step world ' ' of
			Left newWorld -> loop newWorld
			Right (exit, isCheater) -> redraw world >> 
				appendFile logName (msgByExit exit ++ "\n") >> return (exit, isCheater)
				
	where
	maybeAppendFile fileName strings = 
		unless (null strings) $ appendFile fileName $ unwords strings ++ "\n"

-- | add result with given Wave and Level to file 'resName'
addResult :: Int -> Int -> IO ()
addResult wv lvl = do
	s <- catchAll (readFile resName) $ const $ return ""
	let stat = maybe M.empty fst $ listToMaybe $ reads s
	let newStat = M.alter addNew lvl stat
	s `deepseq` writeFile resName (show newStat)
	putStrLn msgAskRes
	ans <- getChar
	_ <- getLine
	unless (ans /= 'y' && ans /= 'Y') $ mapM_ printResult $ M.toList newStat
	where
		addNew Nothing = Just (wv, 1)
		addNew (Just (sm, cnt)) = Just (sm + wv, cnt + 1)
		printResult :: (Int, (Int, Int)) -> IO ()
		printResult (level, (sm, cnt)) = let
			avg = fromIntegral sm / fromIntegral cnt :: Float in
			putStrLn $ "Level: " ++ show level ++ ". Reached: " ++ show cnt
				++ " times. Average wave: " ++ show avg ++ "."

-- | choose all parameters and start or load the game
main :: IO ()
main = do
	save <- catchAll (readFile saveName) $ const $ return ""
	unless (null save) $ putStrLn msgAskLoad
	ans <- if null save then return 'n' else do
		c <- getChar
		_ <- getLine
		return c
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
			then catchAll (return $ Just $ loadWorld $ read save) $ const $ return Nothing
			else do
				mapgen <- showMapChoice
				(char, isCheater) <- showCharChoice
				writeFile logName ""
				return $ Just $ initWorld mapgen char username isCheater gen
		timeBegin <- getCurrentTime
		case maybeWorld of
			Nothing -> endWin >> putStrLn msgLoadErr
			Just world ->
				initScr >> initCurses >> startColor >> initColors >>
				keypad stdScr True >> echo False >>
				cursSet CursorInvisible >> 
				catchAll (do
					(exit, isCheater) <- loop world 
					endWin
					timeEnd <- getCurrentTime
					putStr $ msgByExit exit ++ "\nTime in game: " ++
						renderSecs (round $ diffUTCTime timeEnd timeBegin) ++
						"\n"
					if isCheater
					then putStr $ msgCheater ++ "\n"
					else case exit of
						ExitSave -> return ()
						ExitQuit wv lvl -> do
							putStr $ "Level: " ++ show lvl ++ "\n"
							addResult wv lvl
						Die wv lvl -> do
							putStr $ "Level: " ++ show lvl ++ "\n"
							addResult wv lvl
					)
				(\e -> endWin >> putStrLn (msgGameErr ++ show e))
