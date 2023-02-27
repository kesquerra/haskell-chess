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
        show g = "\ESC[2J" ++ banner ++ show (board g) ++ stateString g
    
    stateString :: Game -> String
    stateString g = case state g of
        State.Check _ -> "\nCheck!\n" ++ turnString g
        State.Checkmate -> "\nCheckmate!\n"
        Stalemate -> "\nStalemate!\n"
        _ -> "\n" ++ turnString g
    
    turnString :: Game -> String
    turnString g = "Turn: " ++ show (turn g) ++ "\n" ++
                "Legal Moves: " ++ intercalate ", " (stringMoves $ turnPlayer g)

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

    getMoves :: Game -> Game
    getMoves (G b ms ps c (State.Check m)) = updateColorMoves (G b ms ps c (State.Check m)) (endCheckMoves m)
    getMoves g = updateColorMoves g getColorMovesList
    

    playMove :: Game -> Move -> Game
    playMove (G b ms (Player ms1 c1, Player ms2 c2) c _) m = getMoves $ G (addMove b m) (m:ms) (Player ms1 c1, Player ms2 c2) (opponent c) (moveState m)

    getMove :: Game -> IO Move
    getMove g = do
        putStrLn "Enter the move you wish to play."
        l <- getLine
        let move = moveFromString l (turnMoves g)
        case move of
            Nothing -> do print g; putStrLn "Error: Invalid Position"; getMove g
            Just v -> return v

    playerTurn :: Game -> IO Game
    playerTurn g = do 
        move <- getMove g
        return $ playMove g move

    checkmate :: Game -> Bool
    checkmate g = null (turnMoves g)

    changeState :: Game -> State -> Game
    changeState (G b ms ps c _) = G b ms ps c

    getState :: Game -> IO State
    getState g = return (state g)