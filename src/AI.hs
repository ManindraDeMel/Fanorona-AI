{-|
Module      : AI
Description : AIs for Fanorona
Copyright   : (c) 2022 ANU and Your Name Here
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
ais = [ ("default", NoLookahead (firstLegalMove COMP1100))
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




heuristicFunction :: GameState -> Int -> Int
heuristicFunction (State _ _ _ [] _) 0 = 0
heuristicFunction (State (GameOver _) _ _ _ _) _ = 0
heuristicFunction (State t@(Turn p) c (x,y) (b:bs) h) a = heuristicHelper b p a x y + heuristicFunction (State t c (x,y) bs h) a + 1
heuristicFunction _ _ = error"Should not match"

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