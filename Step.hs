module Step where

import Data
import Move
import Utils4step
import Object
import ObjectOverall
import Changes
import Utils4all
import Utils4mon

import UI.HSCurses.Curses (Key(..))
import Data.List (sort)
import Data.Set (toList)
import System.Random (StdGen)

step :: World -> Key -> Maybe World
step world c =
	if alive $ getFirst world
	then
		if (time $ getFirst world) > 0
		then Just $ cycleWorld $ tickDown world 
		else if isPlayerNow world
		then
			case action world of
			' ' -> justStep world c
			'q' ->
				let (toQuaff, correct) = quaffFirst c world in
				if correct
				then Just $ newWaveIf toQuaff
				else Just toQuaff
			'r' ->
				let (toRead, correct) = readFirst c world in
				if correct
				then Just $ newWaveIf toRead
				else Just toRead
			'z' ->
				Just $ addDefaultMessage "In what direction?" $ changeAction 'Z' 
				$ world {prevAction = fromKey c}
			'Z' ->
				let (toZap, correct) = zapFirst c world in
				if correct
				then Just $ newWaveIf toZap
				else Just toZap
			't' ->
				if (c == KeyChar '-')
				then
					let (toUntrap, correct) = untrapFirst world in
					if correct
					then Just $ newWaveIf toUntrap
					else Just toUntrap
				else
					let (toTrap, correct) = trapFirst c world in
					if correct
					then Just $ newWaveIf toTrap
					else Just toTrap
			'w' -> 
				let (toWield, correct) = wieldFirst c world in
				if correct
				then Just $ newWaveIf toWield
				else Just toWield
			'f' ->
				Just $ addDefaultMessage "In what direction?" $ changeAction 'F' 
				$ world {prevAction = fromKey c}
			'F' ->
				let (toFire, correct) = fireFirst c world in
				if correct
				then Just $ newWaveIf toFire
				else Just toFire
			'd' ->
				let (toDrop, correct) = dropFirst c world False in
				if correct
				then Just $ newWaveIf toDrop
				else Just toDrop
			'i' ->
				if c == KeyChar '\n' || c == KeyChar ' '
				then Just $ changeAction ' ' world
				else Just world
			',' ->
				if c == KeyChar '\n'
				then
					let maybePick = pickFirst world in
					case maybePick of
						(Nothing, s) ->
							let cleanChangePick = foldl (.) id 
								$ map (changePickFirst . KeyChar) $ toList $ toPick world
							in Just $ cleanChangePick $ addDefaultMessage s $ changeAction ' ' world
						(Just pick, _) -> Just $ newWaveIf pick
				else Just $ changePickFirst c world
			_ -> Just $ addMessage ("You are cheater!", mAGENTA) world
		else
			let newMWorld = aiNow world x y
			in Just $ newWaveIf newMWorld
	else
		if (name $ getFirst world) == "You"
		then Nothing
		else
			let (deadMonster, newStdGen) = addDeathDrop (getFirst world) (stdgen world)
			in Just $ changeGen newStdGen $ remFirst $ dropAll $ changeMon deadMonster
				$ addMessage (name (getFirst world) ++ " die!", cYAN) world
	where
		AI aiNow = ai $ getFirst world
		(x, y) = coordsPlayer world
		
justStep :: World -> Key -> Maybe World
justStep world c = case dir c of
	Just (dx, dy) -> Just $ newWaveIf $ moveFirst world dx dy
	Nothing -> case c of
		KeyChar 'q' ->
			Just $ addDefaultMessage ("What do you want to drink? ["
			 ++ listOfValidChars isPotion world ++ "]") 
			 $ changeAction 'q' world
		KeyChar 'r' ->
			Just $ addDefaultMessage ("What do you want to read? ["
			 ++ listOfValidChars isScroll world ++ "]") 
			 $ changeAction 'r' world
		KeyChar 'z' ->
			Just $ addDefaultMessage ("What do you want to zap? ["
			 ++ listOfValidChars isWand world ++ "]") 
			 $ changeAction 'z' world
		KeyChar 'd' ->
			Just $ addDefaultMessage ("What do you want to drop? ["
			 ++ listOfValidChars (const True) world ++ "]") 
			 $ changeAction 'd' world
		KeyChar 't' ->
			Just $ addDefaultMessage ("What do you want to set? ["
			 ++ listOfValidChars isTrap world ++ "] or - to untrap") 
			 $ changeAction 't' world
		KeyChar 'w' -> 
			Just $ addDefaultMessage ("What do you want to wield? ["
			 ++ listOfValidChars (\x -> isLauncher x || isWeapon x) world ++ "]") 
			 $ changeAction 'w' world
		KeyChar 'f' ->
			Just $ addDefaultMessage ("What do you want to fire? ["
			 ++ listOfValidChars isMissile world ++ "]") 
			 $ changeAction 'f' world
		KeyChar 'i' ->
			Just $ changeAction 'i' world
		KeyChar ',' ->
			Just $ changeAction ',' world
		_  ->
			Just $ addMessage ("Unknown action!", yELLOW) world
