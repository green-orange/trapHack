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
import AI (randomAI)

import UI.HSCurses.Curses (Key(..))
import Data.Set (empty)
import Data.Maybe (isJust)
import System.Random (randomR)

step :: World -> Key -> Either World String
step world c = 
	if alive mon
	then
		if isPlayerNow world && not stun
		then case action world of
			' ' -> justStep world c
			'q' ->
				doIfCorrect $ quaffFirst c world
			'r' ->
				doIfCorrect $ readFirst c world
			'z' ->
				Left $ addDefaultMessage "In what direction?" $ changeAction 'Z' 
				$ world {prevAction = fromKey c}
			'Z' ->
				doIfCorrect $ zapFirst c world
			't' ->
				if (c == KeyChar '-')
				then
					doIfCorrect $ untrapFirst world 
				else
					doIfCorrect $ trapFirst c world
			'f' ->
				Left $ addDefaultMessage "In what direction?" $ changeAction 'F' 
				$ world {prevAction = fromKey c}
			'F' ->
				doIfCorrect $ fireFirst c world
			'd' ->
				doIfCorrect $ dropFirst c world False
			'i' ->
				if c == KeyChar '\n' || c == KeyChar ' '
				then Left $ changeAction ' ' world
				else Left world
			'D' ->
				if c == KeyChar '\n' || c == KeyChar ' '
				then case dropManyFirst world of
					Nothing ->
						Left $ changeChars empty $ changeAction ' ' world
					Just w -> Left $ newWaveIf w
				else Left $ changeChar c world
			',' ->
				if c == KeyChar '\n' || c == KeyChar ' '
				then case pickFirst world of
					(Nothing, s) ->
						Left $ changeChars empty $ addDefaultMessage s 
						$ changeAction ' ' world
					(Just pick, _) -> Left $ newWaveIf pick
				else Left $ changeChar c world
			'E' -> case c of
				KeyDown -> Left $ downshift world
				KeyUp -> Left $ upshift world
				KeyLeft -> Left $ decslot world
				KeyRight -> Left $ incslot world
				KeyChar '\n' -> Left $ changeAction 'e' world
				KeyChar '\ESC' -> Left $ changeAction ' ' world
				_ -> Left world
			'e' ->
				doIfCorrect $ bindFirst c world
			'C' ->
				if c == KeyChar 'y' || c == KeyChar 'Y'
				then Left $ callUpon world
				else Left $ changeAction ' ' world
			'?' ->
				if c == KeyChar '.'
				then Left $ getInfo world
				else case dir c of
					Nothing -> Left world
					Just (dx, dy) -> Left $ world {xInfo = xInfo world + dx, 
						yInfo = yInfo world + dy}
			_ -> Left $ addMessage ("You are cheater!", mAGENTA) 
				$ changeAction ' ' world
		else
			let newMWorld = aiNow x y world
			in Left $ newWaveIf newMWorld
	else
		if name mon == "You"
		then Right $ "You died on the " ++ numToStr (wave world - 1) ++ " wave."
		else
			let (deadMonster, newStdGen) = addDeathDrop (mon) (stdgen world)
			in Left $ changeGen newStdGen $ remFirst $ dropAll $ changeMon deadMonster
				$ addMessage (name mon ++ " die!", cYAN) world
	where
		stun = (isJust $ temp mon !! fromEnum Stun) ||
			(isJust $ temp mon !! fromEnum Conf) && 5*p > 1
		p::Float
		(p, g) = randomR (0.0, 1.0) $ stdgen world
		mon = getFirst world
		AI aiNow = if stun then AI randomAI else ai mon
		(xR, g1) = randomR (0, maxX) g
		(yR, _) = randomR (0, maxY) g1 
		(x, y) = case closestPlayerChar (xFirst world) (yFirst world) world of
			Just (xP, yP) -> (xP, yP)
			Nothing -> (xR, yR)
		
justStep :: World -> Key -> Either World String
justStep world c = case dir c of
	Just (dx, dy) -> Left $ newWaveIf $ moveFirst dx dy world
	Nothing -> case c of
		KeyChar 'Q' -> 
			if wave world == 1
			then Right "You quit. Do not pass go. Do not collect 200 zorkmids."
			else Right $ "You quit on the " ++ numToStr (wave world - 1) ++ " wave."
		KeyChar 'q' ->
			Left $ addDefaultMessage ("What do you want to drink? ["
			 ++ listOfValidChars isPotion world ++ "]") 
			 $ changeAction 'q' world
		KeyChar 'r' ->
			Left $ addDefaultMessage ("What do you want to read? ["
			 ++ listOfValidChars isScroll world ++ "]") 
			 $ changeAction 'r' world
		KeyChar 'z' ->
			Left $ addDefaultMessage ("What do you want to zap? ["
			 ++ listOfValidChars isWand world ++ "]") 
			 $ changeAction 'z' world
		KeyChar 'd' ->
			Left $ addDefaultMessage ("What do you want to drop? ["
			 ++ listOfValidChars (const True) world ++ "]") 
			 $ changeAction 'd' world
		KeyChar 'D' ->
			Left $ changeAction 'D' world
		KeyChar 't' ->
			Left $ addDefaultMessage ("What do you want to set? ["
			 ++ listOfValidChars isTrap world ++ "] or - to untrap") 
			 $ changeAction 't' world
		KeyChar 'f' ->
			Left $ addDefaultMessage ("What do you want to fire? ["
			 ++ listOfValidChars isMissile world ++ "]") 
			 $ changeAction 'f' world
		KeyChar 'E' ->
			Left $ changeAction 'E' world {shift = 0}
		KeyChar 'i' ->
			Left $ changeAction 'i' world
		KeyChar ',' ->
			Left $ changeAction ',' world
		KeyChar 'C' ->
			Left $ changeAction 'C' $ addDefaultMessage
				"Do you really want to call upon the next wave? (y/N)" world
		KeyChar '\n' -> Left world
		KeyChar '?' ->
			Left $ changeAction '?' $ addDefaultMessage
				"Pick an object and press ." world {xInfo = xFirst world,
				 yInfo = yFirst world}
		_  ->
			Left $ addMessage ("Unknown action!", yELLOW) world
