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

import UI.HSCurses.Curses (Key(..))
import Data.List (sort)
import Data.Set (toList)
import System.Random (StdGen)

step :: World -> Key -> Either World String
step world c =
	if alive $ getFirst world
	then
		if (time $ getFirst world) > 0
		then Left $ cycleWorld $ tickDown world 
		else if isPlayerNow world
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
			'w' -> 
				doIfCorrect $ wieldFirst c world
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
			',' ->
				if c == KeyChar '\n'
				then
					let maybePick = pickFirst world in
					case maybePick of
						(Nothing, s) ->
							let cleanChangePick = foldl (.) id 
								$ map (changePickFirst . KeyChar) $ toList $ toPick world
							in Left $ cleanChangePick $ addDefaultMessage s $ changeAction ' ' world
						(Just pick, _) -> Left $ newWaveIf pick
				else Left $ changePickFirst c world
			_ -> Left $ addMessage ("You are cheater!", mAGENTA) world
		else
			let newMWorld = aiNow world x y
			in Left $ newWaveIf newMWorld
	else
		if (name $ getFirst world) == "You"
		then Right $ "You died on the " ++ numToStr (wave world - 1) ++ " wave."
		else
			let (deadMonster, newStdGen) = addDeathDrop (getFirst world) (stdgen world)
			in Left $ changeGen newStdGen $ remFirst $ dropAll $ changeMon deadMonster
				$ addMessage (name (getFirst world) ++ " die!", cYAN) world
	where
		AI aiNow = ai $ getFirst world
		(x, y) = coordsPlayer world
		
justStep :: World -> Key -> Either World String
justStep world c = case dir c of
	Just (dx, dy) -> Left $ newWaveIf $ moveFirst world dx dy
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
		KeyChar 't' ->
			Left $ addDefaultMessage ("What do you want to set? ["
			 ++ listOfValidChars isTrap world ++ "] or - to untrap") 
			 $ changeAction 't' world
		KeyChar 'w' -> 
			Left $ addDefaultMessage ("What do you want to wield? ["
			 ++ listOfValidChars (\x -> isLauncher x || isWeapon x) world ++ "]") 
			 $ changeAction 'w' world
		KeyChar 'f' ->
			Left $ addDefaultMessage ("What do you want to fire? ["
			 ++ listOfValidChars isMissile world ++ "]") 
			 $ changeAction 'f' world
		KeyChar 'i' ->
			Left $ changeAction 'i' world
		KeyChar ',' ->
			Left $ changeAction ',' world
		_  ->
			Left $ addMessage ("Unknown action!", yELLOW) world
