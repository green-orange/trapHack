module IO.Step where

import Data.Const
import Data.World
import Data.Define
import Utils.Changes
import Utils.Monsters
import Utils.Step
import Utils.Items
import Items.Items
import Items.ItemsOverall
import Items.Craft
import Monsters.Move
import Monsters.AI (runAI)
import Monsters.AIrepr
import IO.Messages
import IO.Colors
import IO.Texts

import Data.Set (empty)
import Data.Maybe (isJust)
import System.Random (randomR)
import Data.Char (isSpace)

step :: World -> Char -> Either World String
step world c
	| alive mon = 
		if isPlayerNow world && not stun
		then case action world of
			AfterSpace -> justStep world c
			Move -> justStep world c
			Quaff -> doIfCorrect $ quaffFirst c world
			Read -> doIfCorrect $ readFirst c world
			Zap2 -> doIfCorrect $ zapFirst c world
			Fire2 -> doIfCorrect $ fireFirst c world
			Drop -> doIfCorrect $ dropFirst c world False
			Bind -> doIfCorrect $ bindFirst c world
			Eat -> doIfCorrect $ eatFirst c world
			Craft -> doIfCorrect $ craftByChar c world
			Zap1 -> Left $ addDefaultMessage msgAskDir 
				$ changeAction Zap2 $ world {prevAction = c}
			SetTrap ->
				if c == '-'
				then doIfCorrect $ untrapFirst world 
				else doIfCorrect $ trapFirst c world
			Fire1 -> Left $ addDefaultMessage msgAskDir 
				$ changeAction Fire2 $ world {prevAction = c}
			Inventory ->
				if isSpace c
				then Left $ changeAction Move world
				else Left world
			DropMany ->
				if isSpace c
				then case dropManyFirst world of
					Nothing ->
						Left $ changeChars empty $ changeAction Move world
					Just w -> Left $ newWaveIf w
				else Left $ changeChar c world
			Pick ->
				if isSpace c
				then case pickFirst world of
					(Nothing, s) ->
						Left $ changeChars empty $ addDefaultMessage s 
						$ changeAction Move world
					(Just pick, _) -> Left $ newWaveIf pick
				else Left $ changeChar c world
			Equip
				| isSpace c -> Left $ changeAction Bind world
				| c == '\ESC' -> Left $ changeAction Move world
				| otherwise -> case dir c of
					Nothing -> Left world
					Just (dx, dy) -> Left $ changeSlotOn dx 
						$ changeShiftOn dy world
			Call ->
				if c == 'y' || c == 'Y'
				then Left $ callUpon world
				else Left $ changeAction Move world
			Split1 -> Left $ addDefaultMessage msgPutSize 
				$ changeAction Split2 $ world {prevAction = c, numToSplit = 0}
			Split2 -> 
				if isSpace c
				then Left $ splitFirst world {action = Move}
				else Left $ addNumber c world
			Info ->
				if c == '.'
				then Left $ getInfo world
				else case dir c of
					Nothing -> Left world
					Just (dx, dy) -> Left $ world {xInfo = xInfo world + dx, 
						yInfo = yInfo world + dy}
			Options -> case c of
				'a' -> Left $ world {action = Move, showMode = ColorHeight}
				'b' -> Left $ world {action = Move, showMode = ColorMonsters}
				'c' -> Left $ world {action = Move, showMode = NoHeight}
				_   -> Left $ world {action = Move, message = [(msgUnkOpt, dEFAULT)]}
			_ -> Left $ addMessage (msgCheater, mAGENTA)
				$ changeAction Move world
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
	Just (dx, dy) -> doIfCorrect $ moveFirst dx dy world
	Nothing ->
		if isSpace c then Left $ changeAction AfterSpace world else case c of
		'D' -> Left $ changeAction DropMany world
		'i' -> Left $ changeAction Inventory world
		',' -> Left $ changeAction Pick world
		'S' -> Left $ changeAction Save world
		'P' -> Left $ changeAction Previous world
		'&' -> Left $ changeAction Craft world
		'O' -> Left $ changeAction Options world
		'Q' -> 
			if wave world == 1
			then Right msgQuitOnStart
			else Right $ msgQuit $ wave world - 1
		'q' -> actionByKey "quaff" isPotion Quaff world
		'r' -> actionByKey "read" isScroll Read world
		'z' -> actionByKey "zap" isWand Zap1 world
		'd' -> actionByKey "drop" (const True) Drop world
		'f' -> actionByKey "fire" isMissile Fire1 world
		'e' -> actionByKey "eat" isFood Eat world
		's' -> actionByKey "split" (const True) Split1 world
		't' ->
			Left $ addDefaultMessage (msgAsk ++ "set? ["
			 ++ listOfValidChars isTrap world ++ "] or - to untrap") 
			 $ changeAction SetTrap world
		'E' ->
			Left $ changeAction Equip world {shift = 0, slot = minBound :: Slot}
		'C' ->
			Left $ changeAction Call $ addDefaultMessage	msgConfirmCall world
		'?' ->
			Left $ changeAction Info $ addDefaultMessage
				msgInfo world {xInfo = xFirst world,
				 yInfo = yFirst world}
		_  ->
			Left $ addMessage (msgUnkAct ++ show (fromEnum c), yELLOW) world

actionByKey :: String -> (Object -> Bool) -> Action -> World -> Either World a
actionByKey word isType char world = Left $ addDefaultMessage (msgAsk 
	++ word ++ "? [" ++ listOfValidChars isType world ++ "]") 
	$ changeAction char world
