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
    import Convert

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

    addMove :: Board -> Move -> Board
    addMove b (M (Pos sq _) (Pos sq2 _) _) = movePiece b sq sq2  

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
    generateMove b s e = M (getPosition b s) (getPosition b e) Move

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
        Just (Piece c Pawn) -> map (removeForwardPawnCaptures b) (pieceMoves sq (Piece c Pawn)) ++ pawnCaptures b c sq
        Just p -> pieceMoves sq p

    -- need to refactor everything below

    removeForwardPawnCaptures :: Board -> [Maybe Square] -> [Maybe Square]
    removeForwardPawnCaptures _ [] = []
    removeForwardPawnCaptures b (x:xs) = case x of
        Nothing -> removeForwardPawnCaptures b xs
        Just sq -> case getPiece b sq of
            Nothing -> x : removeForwardPawnCaptures b xs
            Just _ -> removeForwardPawnCaptures b xs

    pawnCapture :: Board -> Color -> (Int, Int) -> Square -> Maybe Square
    pawnCapture b col xy sq = moveXY sq xy >>= \sq2 -> getPiece b sq2 >>= \(Piece c _) -> if col /= c then Just sq2 else Nothing

    pawnCaptures :: Board -> Color -> Square -> [[Maybe Square]]
    pawnCaptures b White sq = [[pawnCapture b White (1, 1) sq], [pawnCapture b White (-1, 1) sq]]
    pawnCaptures b Black sq = [[pawnCapture b Black (1, -1) sq], [pawnCapture b Black (-1, -1) sq]]

    getPositionsFromNext :: Board -> Move -> [[Position]]
    getPositionsFromNext b m = possiblePositions (addMove b m) (Pos (square (endPos m)) (piece (startPos m)))

    isCheckMateMove :: Board -> Color -> Move -> Bool
    isCheckMateMove b c m = checkmate (addMove b m) (opponent c) m

    isCheckMove :: Board -> Color -> Move -> Bool
    isCheckMove b c m = or $ possiblePositions (addMove b m) (Pos (square (endPos m)) (piece (startPos m))) >>= \ps -> case parse (many (parseMove (startPos m) c)) ps of
        Nothing -> []
        Just (xs, _) -> return $ any (\m2 -> mtype m2 == Check) xs

    moveSet :: Board -> Position -> Color -> [Position] -> [Move]
    moveSet b pos c ps = case parse (many (parseMove pos c)) ps of
            Nothing -> []
            Just (ms, _) -> map (\m -> if isCheckMove b c m then if isCheckMateMove b c m then swapType m Checkmate else swapType m Check else m) ms

    getKingPosition :: Board -> Color -> Maybe Position
    getKingPosition b c = case parse (many (findKing c)) (positions b) of
        Just (m, _) -> safeHead (catMaybes m)
        Nothing -> Nothing

    getColorPieces :: Board -> Color -> [Position]
    getColorPieces b c = case parse (many (allPieces c)) (positions b) of
        Just (ps, _) -> catMaybes ps
        Nothing -> []

    getColorMoves :: Board -> Color -> [[Move]]
    getColorMoves b c = removeEmpties $ getColorPieces b c >>= \(Pos sq _) -> [generateLegalMoves b c sq]

    getColorMovesList :: Board -> Color -> [Move]
    getColorMovesList b c = concat (getColorMoves b c)
    
    getColorPositions :: Board -> Color -> [[Position]]
    getColorPositions b c = removeEmpties $ getColorPieces b c >>= \(Pos sq _) -> [generateLegalMoves b c sq] >>= \m -> return $ map endPos m

    getAttackLanes :: Board -> Color -> [[Position]]
    getAttackLanes b c = removeEmpties $ getColorPieces b c >>= possiblePositions b >>= 
        \ps -> case parse (many Parser.empty) ps of
            Nothing -> []
            Just (xs, _) -> return xs

    movesOutOfCheck :: Board -> Color -> [Move]
    movesOutOfCheck b c =  case getKingPosition b c of
        Nothing -> []
        Just (Pos sq _) -> Prelude.filter (\m -> square (endPos m) `notElem` map square (concat (getAttackLanes b c))) (generateLegalMoves b c sq)

    getCheckLane :: Board -> Color -> Move -> [[Position]]
    getCheckLane b c m = possiblePositions b (Pos (square (endPos m)) (piece (startPos m))) >>=
        \case
            [] -> return []
            xs -> case parse (many (movable c)) xs of
                Nothing -> []
                Just ([], _) -> return []
                Just (ys, _) -> case last ys of
                    (Pos _ (Just (Piece col King))) -> if c /= col then return (endPos m : init ys) else return []
                    _ -> return []

    blockCheckMoves :: Board -> Color -> Move -> [Move]
    blockCheckMoves b c m = Prelude.filter (\lm -> square (endPos lm) `elem` map square (concat (getCheckLane b (opponent c) m))) (removePieceMove King (concat (getColorMoves b c)))
    
    endCheckMoves :: Move -> Board -> Color -> [Move]
    endCheckMoves m b c = blockCheckMoves b c m ++ movesOutOfCheck b c

    checkmate :: Board -> Color -> Move -> Bool
    checkmate b c m = null $ endCheckMoves m b c

    addMoves :: Board -> [Move] -> Board
    addMoves b [] = b
    addMoves b (m:ms) = addMove (addMoves b ms) m

    testBoard :: Board
    testBoard = addMoves startBoard testMoves 