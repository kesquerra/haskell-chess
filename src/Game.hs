{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Game where
    import Board
    import Move
    import Player
    import Color
    import State
    import Art
    import Data.List ( intercalate )

    data Game = G {
        board :: Board,
        moves :: [Move],
        players :: (Player, Player),
        turn :: Color,
        state :: State
    }

    instance Show Game where
        show g = "\ESC[2J" ++ banner ++ "\n" ++ boardString (turn g) (board g) ++ stateString g
    
    stateString :: Game -> String
    stateString g = case state g of
        State.Check _ -> "Check!\n" ++ turnString g
        State.Checkmate -> "Checkmate!\n"
        Stalemate -> "Stalemate!\n"
        _ -> turnString g
    
    turnString :: Game -> String
    turnString g = show (turn g) ++ "'s legal moves:\n\n" ++ intercalate "\n" (map (intercalate "\t") (stringMoves $ turnPlayer g)) ++ "\n"

    turnPlayer :: Game -> Player
    turnPlayer g = if turn g == White then fst (players g) else snd (players g)

    turnMoves :: Game -> [Move]
    turnMoves g = Player.moves $ turnPlayer g

    initGame :: Game
    initGame = G startBoard [] (Player [] White, Player [] Black) White Ongoing

    newGame :: Game
    newGame = getMoves initGame

    updateColorMoves :: Game -> (Board -> Color -> [Move]) -> Game
    updateColorMoves (G b ms (Player _ c1, Player _ c2) c s) f = if c == c1
        then G b ms (Player (f b c1) c1, Player [] c2) c s
        else G b ms (Player [] c1, Player (f b c2) c) c s

    -- get all possible moves in the position for the color whose turn it is
    getMoves :: Game -> Game
    getMoves (G b ms ps c (State.Check m)) = updateColorMoves (G b ms ps c (State.Check m)) (endCheckMoves m)
    getMoves g = do 
        let nb = updateColorMoves g getColorMovesList
        if null (turnMoves nb) then changeState nb Stalemate else nb
    

    -- add the move to the current board and flip turns
    playMove :: Game -> Move -> Game
    playMove (G b ms (Player ms1 c1, Player ms2 c2) c _) m = getMoves $ G (addMove b m) (m:ms) (Player ms1 c1, Player ms2 c2) (opponent c) (moveState m)

    -- collect move from string input
    getMove :: Game -> IO Move
    getMove g = do
        putStrLn "Enter the move you wish to play."
        l <- getLine
        let move = moveFromString l (turnMoves g)
        case move of
            Nothing -> do print g; putStrLn "Error: Invalid Position"; getMove g
            Just v -> return v

    
    -- get move from input, play on board
    playerTurn :: Game -> IO Game
    playerTurn g = do 
        move <- getMove g
        return $ playMove g move

    noMoves :: Game -> Bool
    noMoves g = null (turnMoves g)

    changeState :: Game -> State -> Game
    changeState (G b ms ps c _) = G b ms ps c

    getState :: Game -> IO State
    getState g = return (state g)