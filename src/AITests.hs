{-|
Module      : AITests
Description : Tests for your AI functions
Copyright   : (c) 2020 Manindra de Mel
License     : AllRightsReserved
-}
module AITests where

import           AI
import           Fanorona
import           Testing

aiTests :: Test
aiTests = TestGroup "AI"
  [
    Test "heuristicFunction" (assertEqual  (heuristic (initialState (4,2))) 7),
    Test "nextGameStates" (assertEqual a b),
    Test "MiniMax" (assertEqual (miniMax (initialState (4,2)) 3) (Move Approach (Location 3 2) (Location 4 2)))
  ]
    where 
    a = nextPossibleGameStates (initialState (4,2))
    b = [
      (Move Approach (Location 5 3) (Location 4 2),Just (State (Turn Player2) None (9,5) 
        [ [Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2],
          [Piece Player2,Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2],
          [Piece Player2,Piece Player1,Piece Player2,Piece Player1,Piece Player1,Piece Player2,Piece Player1,Piece Player2,Piece Player1],
          [Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Empty,Piece Player1,Piece Player1,Piece Player1],
          [Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1]
          ] []))
      ,(Move Approach (Location 3 2) (Location 4 2),Just (State (Turn Player2) None (9,5) 
        [ [Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2],
          [Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2],
          [Piece Player2,Piece Player1,Piece Player2,Empty,Piece Player1,Empty,Piece Player1,Piece Player2,Piece Player1],
          [Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1],
          [Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1]
        ] []))
      ,(Move Approach (Location 4 3) (Location 4 2),Just (State (Turn Player2) None (9,5) 
        [ [Piece Player2,Piece Player2,Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2,Piece Player2,Piece Player2],
          [Piece Player2,Piece Player2,Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2,Piece Player2,Piece Player2],
          [Piece Player2,Piece Player1,Piece Player2,Piece Player1,Piece Player1,Piece Player2,Piece Player1,Piece Player2,Piece Player1],
          [Piece Player1,Piece Player1,Piece Player1,Piece Player1,Empty,Piece Player1,Piece Player1,Piece Player1,Piece Player1],
          [Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1]
        ] []))
      ,(Move Approach (Location 3 3) (Location 4 2),Just (State (Turn Player2) None (9,5) 
        [ [Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2],
          [Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2,Piece Player2],
          [Piece Player2,Piece Player1,Piece Player2,Piece Player1,Piece Player1,Piece Player2,Piece Player1,Piece Player2,Piece Player1],
          [Piece Player1,Piece Player1,Piece Player1,Empty,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1],
          [Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1]
        ] []))
      ,(Move Withdrawal (Location 3 2) (Location 4 2),Just (State (Turn Player2) None (9,5) 
        [ [Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2],
          [Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2],
          [Piece Player2,Piece Player1,Empty,Empty,Piece Player1,Piece Player2,Piece Player1,Piece Player2,Piece Player1],
          [Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1],
          [Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1]
        ] []))
      ]
