module Step where

import Data
import Move
import Utils4step
import Object
import ObjectOverall
import Changes
import Utils4mon
import Messages
import Utils4objects
import Colors
import AI (runAI)
import Texts
import AIrepr
import DataWorld
import DataDef

import Data.Set (empty)
import Data.Maybe (isJust)
import System.Random (randomR)
import Data.Char (isSpace)

step :: World -> Char -> Either World String
step world c
	| alive mon = 
		if isPlayerNow world && not stun
		then case action world of
			' ' -> justStep world c
			'q' -> doIfCorrect $ quaffFirst c world
			'r' -> doIfCorrect $ readFirst c world
			'Z' -> doIfCorrect $ zapFirst c world
			'F' -> doIfCorrect $ fireFirst c world
			'd' -> doIfCorrect $ dropFirst c world False
			'#' -> doIfCorrect $ bindFirst c world
			'e' -> doIfCorrect $ eatFirst c world
			'z' ->
				Left $ addDefaultMessage msgAskDir $ changeAction 'Z' 
				$ world {prevAction = c}
			't' ->
				if c == '-'
				then
					doIfCorrect $ untrapFirst world 
				else
					doIfCorrect $ trapFirst c world
			'f' ->
				Left $ addDefaultMessage msgAskDir $ changeAction 'F' 
				$ world {prevAction = c}
			'i' ->
				if isSpace c
				then Left $ changeAction ' ' world
				else Left world
			'D' ->
				if isSpace c
				then case dropManyFirst world of
					Nothing ->
						Left $ changeChars empty $ changeAction ' ' world
					Just w -> Left $ newWaveIf w
				else Left $ changeChar c world
			',' ->
				if isSpace c
				then case pickFirst world of
					(Nothing, s) ->
						Left $ changeChars empty $ addDefaultMessage s 
						$ changeAction ' ' world
					(Just pick, _) -> Left $ newWaveIf pick
				else Left $ changeChar c world
			'E'
				| isSpace c -> Left $ changeAction '#' world
				| c == '\ESC' -> Left $ changeAction ' ' world
				| otherwise -> case dir c of
					Nothing -> Left world
					Just (dx, dy) -> Left $ changeSlotOn dx 
						$ changeShiftOn dy world
			'C' ->
				if c == 'y' || c == 'Y'
				then Left $ callUpon world
				else Left $ changeAction ' ' world
			'?' ->
				if c == '.'
				then Left $ getInfo world
				else case dir c of
					Nothing -> Left world
					Just (dx, dy) -> Left $ world {xInfo = xInfo world + dx, 
						yInfo = yInfo world + dy}
			_ -> Left $ addMessage (msgCheater, mAGENTA)
				$ changeAction ' ' world
		else
			let newMWorld = runAI aiNow x y peace world
			in Left $ newWaveIf newMWorld
	| name mon == "You" =
		Right $ msgYouDie $ wave world - 1
	| otherwise =
		let (deadMonster, newStdGen) = addDeathDrop mon (stdgen world)
		in Left $ changeGen newStdGen $ remFirst $ dropAll $ changeMon deadMonster
			$ addMessage (name mon ++ " die!", cYAN) world
	where
		stun = (isJust (temp mon !! fromEnum Stun) ||
			isJust (temp mon !! fromEnum Conf) && 5*p > 1) &&
			canWalk mon
		p :: Float
		(p, g) = randomR (0.0, 1.0) $ stdgen world
		mon = getFirst world
		AI aiNow = if stun then AI $ getPureAI RandomAI else ai mon
		(xR, g1) = randomR (0, maxX) g
		(yR, _) = randomR (0, maxY) g1 
		(x, y, peace ) = case closestPlayerChar (xFirst world) 
			(yFirst world) world of
			Just (xP, yP) -> (xP, yP, False)
			Nothing -> (xR, yR, True)
		
justStep :: World -> Char -> Either World String
justStep world c = case dir c of
	Just (dx, dy) -> Left $ newWaveIf $ moveFirst dx dy world
	Nothing
		| isSpace c -> Left world 
		| c `elem` ['D', 'i', ',', 'S'] -> Left $ changeAction c world
		| otherwise -> case c of
		'Q' -> 
			if wave world == 1
			then Right msgQuitOnStart
			else Right $ msgQuit $ wave world - 1
		'q' -> actionByKey "quaff" isPotion 'q' world
		'r' -> actionByKey "read" isScroll 'r' world
		'z' -> actionByKey "zap" isWand 'z' world
		'd' -> actionByKey "drop" (const True) 'd' world
		'f' -> actionByKey "fire" isMissile 'f' world
		'e' -> actionByKey "eat" isFood 'e' world
		't' ->
			Left $ addDefaultMessage (msgAsk ++ "set? ["
			 ++ listOfValidChars isTrap world ++ "] or - to untrap") 
			 $ changeAction 't' world
		'E' ->
			Left $ changeAction 'E' world {shift = 0, slot = minBound :: Slot}
		'C' ->
			Left $ changeAction 'C' $ addDefaultMessage	msgConfirmCall world
		'?' ->
			Left $ changeAction '?' $ addDefaultMessage
				msgInfo world {xInfo = xFirst world,
				 yInfo = yFirst world}
		_  ->
			Left $ addMessage (msgUnkAct ++ show (fromEnum c), yELLOW) world

actionByKey :: String -> (Object -> Bool) -> Char -> World -> Either World a
actionByKey word isType char world = Left $ addDefaultMessage (msgAsk 
	++ word ++ "? [" ++ listOfValidChars isType world ++ "]") 
	$ changeAction char world
