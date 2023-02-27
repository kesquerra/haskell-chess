module State where
    import Move

    data State = Ongoing | Checkmate | Stalemate | Check Move
        deriving Show


    moveState :: Move -> State
    moveState m = case mtype m  of
        Move.Checkmate -> State.Checkmate
        Move.Check -> State.Check m
        _ -> Ongoing