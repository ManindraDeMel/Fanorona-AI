{-|
Module      : AI
Description : AIs for Fanorona
Copyright   : (c) 2022 ANU and Manindra de Mel
License     : AllRightsReserved
-}
module AI where

import           Fanorona
-- | Type of AI functions you can choose to write.
data AIFunc
  = NoLookahead (GameState -> Move)
    -- ^ Simple AIs that do not need lookahead.
  | WithLookahead (GameState -> Int -> Move)
    -- ^ AIs that want to look ahead. The assignment framework will
    -- call the function over and over with increasing integer
    -- arguments @1, 2, 3, ...@ until your AI's time limit is up. 

-- | The table of all AIs that your assignment provides. The AI named
-- "default" in this table is the one your tutor will dedicate most of
-- his or her attention to marking.
ais :: [(String, AIFunc)]
ais = [ ("default", WithLookahead miniMax)
      ]

-- | A very simple AI, which passes whenever it can, and if not,
-- picks the first move returned by the 'legalMoves' function.
-- By default, this function is called on COMP1100 rules, and so
-- will never pass.
-- AIs can rely on the 'legalMoves' list being
-- non-empty; if there were no legal moves, the framework would have
-- ended the game.
firstLegalMove :: Course -> GameState -> Move
firstLegalMove course state = case applyMove course Pass state of
  Nothing -> head (legalMoves state)
  _ -> Pass

miniMax :: GameState -> Int -> Move -- minimax parent function
miniMax (State (GameOver _) _ _ _ _) _ = error"The program shouldn't pass a gameover state to the AI"
miniMax g@(State (Turn p) _ _ _ _) n = fst $ miniMaxH (Pass, Just g) p n n -- abitary initial arguments that are overwritten when searching through the tree

miniMaxH :: (Move, Maybe GameState) -> Player -> Int -> Int -> (Move, Int) -- minimax
miniMaxH (_, Nothing) _ _ _ = error"Should not be called"
miniMaxH (m, Just (State (GameOver (Winner p)) _ _ _ _)) op _ _ = if op == p then (m, 10000) else (m, -100000) -- give reward for winning and punish for losing
miniMaxH (m, Just (State (GameOver Draw) _ _ _ _)) _ _ _ = (m, 0) -- preferably don't draw, try to win. Thus non-negative and not positive either
miniMaxH (m, Just g) _ _ 0 = (m, heuristic g)
miniMaxH (m, Just g@(State (Turn p) _ _ _ _)) originalPlayer originalDepth depth
  | (p == originalPlayer) && (originalDepth == depth) =  maxBySnd nextGameStates
  | (p /= originalPlayer) && (originalDepth == depth) = minBySnd nextGameStates
  | (p == originalPlayer) && (originalDepth /= depth) = (m, (maximum . map snd) nextGameStates)
  | (p /= originalPlayer) && (originalDepth /= depth) = (m, (minimum . map snd) nextGameStates)
    where
      nextGameStates = [miniMaxH ng originalPlayer depth (depth - 1) | ng <- nextPossibleGameStates g]
      maxBySnd :: [(a, Int)] -> (a, Int)
      maxBySnd li = head $ filter (\x -> snd x == maxElem) li
        where maxElem = (maximum . map snd) li
      minBySnd :: [(a, Int)] -> (a, Int)
      minBySnd li = head $ filter (\x -> snd x == minElem) li
        where minElem = (minimum . map snd) li
miniMaxH _ _ _ _ = error"Should not match"

nextPossibleGameStates :: GameState -> [(Move, Maybe GameState)] -- get all the next gamestates and the moves associated to get to that gamestate
nextPossibleGameStates g
  | not (null captureStates) = captureStates -- if there are capturing moves we must do them
  | otherwise = nextGameState' (Just g) legalMoves -- otherwise attempt a legal move (rare occourance)
    where
      captureStates = nextGameState' (Just g) captures

nextGameState' :: Maybe GameState -> (GameState -> [Move]) -> [(Move, Maybe GameState)]
nextGameState' Nothing _ = [] -- this and the line below match the same case, kinda...
nextGameState' (Just (State (GameOver _) _ _ _ _ )) _ = []
nextGameState' (Just g@(State (Turn p) None _ _ _ )) f = [(x, applyMove COMP1130 x g) | x <- getPlayerSpecificMoves]
  where
    getPlayerSpecificMoves = movesByPlayer p g f
nextGameState' a@(Just g@(State (Turn p) _ _ _ _ )) f = [(x, applyMove COMP1130 x g) | x <- getPlayerSpecificMoves] ++ [(Pass, a)]
  where
    getPlayerSpecificMoves = movesByPlayer p g f

movesByPlayer :: Player -> GameState -> (GameState -> [Move]) -> [Move] -- filter out moves based on the player passed
movesByPlayer p g f = filter (comparePoints allLocations) possibleMoves
  where
    possibleMoves = f g -- legalMoves or captures can be used here
    allLocations = locationsOf (Piece p) g

    comparePoints :: [Location] -> Move -> Bool
    comparePoints l (Move _ a _) = a `elem` l
    comparePoints _ Pass = False

heuristic :: GameState -> Int -- Heuristic based on advantagous position (centre of the board) and the difference between the pieces
heuristic = heuristicFunction 0

heuristicFunction :: Int -> GameState -> Int
heuristicFunction _ (State (GameOver _) _ _ _ _) = 0
heuristicFunction _ (State _ _ _ [] _) = 0
heuristicFunction a (State t@(Turn p) c (x,y) (b:bs) h) = heuristicHelper b p a x y + heuristicFunction (a + 1) (State t c (x,y) bs h)

heuristicHelper :: [Square] -> Player -> Int -> Int -> Int -> Int -- add (+2) for all pieces which are the AI's and an extra 1 for pieces with 8 possible directions. (-2) for enemy and (-1) for advantagous positions
heuristicHelper [] _ _ _ _ = 0
heuristicHelper ls@(x:xs) p currentRow width height
  | onRightBorder = compareSquarePlayer x p True
  | onYBorder = compareSquarePlayer x p True + heuristicHelper xs p currentRow width height
  | onLeftBorder = compareSquarePlayer x p True + heuristicHelper xs p currentRow width height
  | otherwise = compareSquarePlayer x p False + heuristicHelper xs p currentRow width height
    where
      onLeftBorder = length ls == width
      onRightBorder = length ls == 1
      onYBorder = currentRow == 1 || currentRow == height

compareSquarePlayer :: Square -> Player -> Bool -> Int
compareSquarePlayer Empty _ _ = 0
compareSquarePlayer (Piece a) b onBorder
  | onBorder && (a == b) = 2
  | not onBorder && (a == b) = 3
  | onBorder && (a /= b) = -2
  | otherwise = -3