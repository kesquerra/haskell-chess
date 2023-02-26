{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}

module Board where
    import Square
    import Color
    import Art
    import Position
    import Piece
    import Move
    import Parser
    import Control.Applicative
    import Data.Maybe

    newtype Board = B {positions :: [Position]}


    instance Semigroup Board where
        (<>) b1 b2 = B (positions b1 ++ positions b2)

    instance Monoid Board where
        mempty = B []

    instance Show Board where
        show = boardString


    emptyBoard :: Board
    emptyBoard = B [Pos (Sq x y) Nothing | x <- [1..8], y <- [1..8]]

    startBoard :: Board
    startBoard = B (backRank White ++ frontRank White ++ concat [emptyRank i | i <- [3..6]] ++ frontRank Black ++ backRank Black)

    flipBoard :: Board -> Board
    flipBoard (B []) = B []
    flipBoard (B b) = flipBoard (B (drop 8 b)) <> B (take 8 b)

    boardString :: Board -> String
    boardString b = "\n" ++ horizontalLine ++ emptyLineWithBorder ++
        (positions (flipBoard b) >>= \(Pos sq p) ->
        if file sq == 1
            then show (rank sq) ++ " | " ++ positionString (Pos sq p) 
            else positionString (Pos sq p))
        ++ horizontalLine ++ tab ++ tab ++ fileLabels

    updateSquare :: Board -> Square -> Maybe Piece -> Board
    updateSquare b sq p = do 
        let (xs, ys) = splitAt (index sq) (positions b)
        B (xs ++ [Pos sq p] ++ tail ys)

    clearSquare :: Board -> Square -> Board
    clearSquare b sq = updateSquare b sq Nothing

    addPieceToSquare :: Board -> Square -> Piece -> Board
    addPieceToSquare b sq p = updateSquare b sq $ Just p

    getPiece :: Board -> Square -> Maybe Piece
    getPiece (B ps) sq = piece (ps !! index sq)

    movePiece :: Board -> Square -> Square -> Board
    movePiece b s e = case getPiece b s of 
        Nothing -> b
        Just p -> addPieceToSquare (clearSquare b s) e p
    
    getPosition :: Board -> Square -> Position
    getPosition b sq = Pos sq (getPiece b sq)

    generateMove :: Board -> Square -> Square -> Move
    generateMove b s e = M (getPosition b s) (getPosition b e) False False False False

    possiblePositions :: Board -> Position -> [[Position]]
    possiblePositions b (Pos sq (Just _)) = getMoveSquares b sq >>=
        \sqs -> return $ mapMaybe (sqToPos b) sqs
    possiblePositions _ _ = []

    sqToPos :: Board -> Maybe Square -> Maybe Position
    sqToPos b sq = case sq of
        Nothing -> Nothing
        Just v -> Just $ Pos v (getPiece b v)

    generateLegalMoves :: Board -> Color -> Square -> [Move]
    generateLegalMoves b c sq = possiblePositions b (getPosition b sq) >>= 
        \m -> moveSet b (getPosition b sq) c m 

    getMoveSquares :: Board -> Square -> [[Maybe Square]]
    getMoveSquares b sq = case getPiece b sq of
        Nothing -> []
        Just (Piece _ t) -> pieceMoves sq t

    swapForCheck :: Color -> Move -> [Position] -> Maybe Move
    swapForCheck c (M p1 p2 _ x y z) ps = case parse (many (checkMove p2 c)) ps of
            Nothing -> Nothing
            Just (ms, _) -> if not (null ms) then Just (M p1 p2 True x y z) else Nothing

    isCheckMove :: Board -> Color -> Move -> [Move]
    isCheckMove b c (M p1 p2 w x y z) = mapMaybe (swapForCheck c (M p1 p2 w x y z)) (possiblePositions b p2)


    getSwap :: [a] -> Maybe a
    getSwap ms
        | null ms = Nothing
        | otherwise = Just $ head ms

    kingHasMoves :: Board -> Color -> Bool
    kingHasMoves b c = case getKingPosition b c of
        Just (Pos sq _) -> not $ null (generateLegalMoves b c sq)
        _ -> False

    swapCheckMateMove :: Board -> Color -> Move -> Move
    swapCheckMateMove b White (M p1 p2 w x y z)
        | kingHasMoves b Black = (M p1 p2 w x y z)
        | otherwise = swapMove isCheckMove b White (M p1 p2 w True y z)
    swapCheckMateMove b Black (M p1 p2 w x y z)
        | kingHasMoves b White = (M p1 p2 w x y z)
        | otherwise = swapMove isCheckMove b Black (M p1 p2 w True y z)

    swapMove :: (Board -> Color -> Move -> [Move]) -> Board -> Color -> Move -> Move
    swapMove f b c m = fromMaybe m (getSwap (f b c m))

    moveSet :: Board -> Position -> Color -> [Position] -> [Move]
    moveSet b pos c ps = case parse (many (captureMove pos c <|> move pos)) ps of
            Nothing -> []
            Just (ms, _) -> map (swapCheckMateMove b c . swapMove isCheckMove b c) ms

    getKingPosition :: Board -> Color -> Maybe Position
    getKingPosition b c = case parse (many (findKing c)) (positions b) of
        Just (m, _) -> getSwap (catMaybes m)
        Nothing -> Nothing