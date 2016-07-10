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
import IO.Show(castEnum)

import Data.Set (empty)
import Data.Maybe (isJust, fromMaybe)
import System.Random (randomR)
import Data.Char (isSpace)
import Data.Functor ((<$>))

-- | converts a char from the player to some action or end of game
step :: World -> Char -> Either World String
step world c
	| alive mon = 
		if isPlayerNow world
		then case action world of
			AfterSpace -> justStep c stun world
			Move -> justStep c stun world
			Quaff -> doIfCorrect $ quaffFirst c world
			Read -> doIfCorrect $ readFirst c world
			Zap2 -> doIfCorrect $ zapFirst c world
			Fire2 -> doIfCorrect $ fireFirst c world
			Use2 -> doIfCorrect $ useFirst c world
			Drop -> doIfCorrect $ dropFirst c world False
			Bind -> doIfCorrect $ bindFirst c world
			Eat -> doIfCorrect $ eatFirst c world
			Craft -> doIfCorrect $ craftByChar c world
			Zap1 -> Left $ addDefaultMessage msgAskDir world
				{prevAction = c, action = Zap2}
			SetTrap ->
				if c == '-'
				then doIfCorrect $ untrapFirst world 
				else doIfCorrect $ trapFirst c world
			Fire1 -> Left $ addDefaultMessage msgAskDir world
				{prevAction = c, action = Fire2}
			Use1 -> Left $ addDefaultMessage msgAskDir world
				{prevAction = c, action = Use2}
			Inventory ->
				if isSpace c || c == '\ESC'
				then Left $ world {action = Move}
				else Left world
			DropMany ->
				if isSpace c
				then Left $ fromMaybe (world {action = Move, chars = empty})
					$ changeChar c <$> dropManyFirst world
				else Left $ changeChar c world
			Pick ->
				if isSpace c
				then case pickFirst world of
					(Nothing, s) -> Left $ addDefaultMessage s
						world {action = Move, chars = empty}
					(Just pick, _) -> Left $ newWaveIf pick
				else Left $ changeChar c world
			Equip
				| isSpace c -> Left world {action = Bind}
				| c == '\ESC' -> Left world {action = Move}
				| otherwise -> case dir c of
					Nothing -> Left world
					Just (dx, dy) -> Left $ changeSlotOn dx 
						$ changeShiftOn dy world
			Call ->
				if c == 'y' || c == 'Y'
				then Left $ callUpon world
				else Left world {action = Move}
			Split1 -> Left $ addDefaultMessage msgPutSize world
				{prevAction = c, numToSplit = 0, action = Split2}
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
			Options -> Left $ (case c of
				'a' -> world {colorHeight = Absolute}
				'b' -> world {colorHeight = Relative}
				'c' -> world {colorHeight = NoColor}
				'd' -> world {symbolHeight = Numbers}
				'e' -> world {symbolHeight = SymbolHeight $ castEnum '.'}
				'f' -> world {symbolHeight = SymbolHeight filledSquare}
				_   -> world {message = [(msgUnkOpt, defaultc)]}) {action = Move}
			_ -> putWE $ "step: action = " ++ show (action world)
		else
			let newMWorld = runAI aiNow x y peace world
			in Left $ newWaveIf newMWorld
	| name mon == "You" =
		Right $ msgYouDie $ wave world - 1
	| otherwise =
		let (deadMonster, newStdGen) = addDeathDrop mon (stdgen world)
		in Left $ remFirst $ dropAll $ changeMon deadMonster
			$ addMessage (name mon ++ " die!", cyan) world {stdgen = newStdGen}
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
		(x, y, peace) = case closestPlayerChar (xFirst world) 
			(yFirst world) world of
			Just (xP, yP) -> (xP, yP, False)
			Nothing -> (xR, yR, True)

-- | case when action is just move
justStep :: Char -> Bool -> World -> Either World String
justStep c stun world = case dir c of
	Just (dx, dy) -> doIfCorrect $ moveFirst dx' dy' world {stdgen = g'} where
		(px, g) = randomR (-1, 1) $ stdgen world
		(py, g') = randomR (-1, 1) g
		(dx', dy') = if stun then (px, py) else (dx, dy)
	Nothing ->
		if isSpace c then Left $ world {action = AfterSpace} else case c of
		'D' -> Left world {action = DropMany}
		'i' -> Left world {action = Inventory}
		',' -> Left world {action = Pick}
		'S' -> Left world {action = Save}
		'P' -> Left world {action = Previous}
		'&' -> Left world {action = Craft}
		'O' -> Left world {action = Options}
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
		'U' -> actionByKey "use" isTool Use1 world
		't' ->
			Left $ addDefaultMessage (msgAsk ++ "set? ["
			 ++ listOfValidChars isTrap world ++ "] or - to untrap") 
			 world {action = SetTrap}
		'E' ->
			Left world {shift = 0, slot = minBound :: Slot, action = Equip}
		'C' ->
			Left $ addDefaultMessage msgConfirmCall world {action = Call}
		'?' ->
			Left $ addDefaultMessage
				msgInfo world {xInfo = xFirst world,
				 yInfo = yFirst world, action = Info}
		_  ->
			Left $ addMessage (msgUnkAct ++ show (fromEnum c), yellow) world

-- | action with key choose and 'action' changing
actionByKey :: String -> (Object -> Bool) -> Action -> World -> Either World a
actionByKey word isType char world = Left $ addDefaultMessage (msgAsk 
	++ word ++ "? [" ++ listOfValidChars isType world ++ "]") 
	world {action = char}
