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
        show = boardString White

    emptyBoard :: Board
    emptyBoard = B [Pos (Sq x y) Nothing | x <- [1..8], y <- [1..8]]

    startBoard :: Board
    startBoard = B (backRank White ++ frontRank White ++ concat [emptyRank i | i <- [3..6]] ++ frontRank Black ++ backRank Black)

    flipBoard :: Board -> Board
    flipBoard (B []) = B []
    flipBoard (B b) = flipBoard (B (drop 8 b)) <> B (take 8 b)

    boardString :: Color -> Board ->  String
    boardString c b = "\n" ++ horizontalLine ++ emptyLineWithBorder ++(positions (if c == White then flipBoard b else b) >>=
        \(Pos sq p) -> if file sq == 1
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
    sqToPos b sq = sq >>= \v -> Just $ Pos v (getPiece b v)

    generateLegalMoves :: Board -> Color -> Square -> [Move]
    generateLegalMoves b c sq = possiblePositions b (getPosition b sq) >>= moveSet b (getPosition b sq) c
    
    getMoveSquares :: Board -> Square -> [[Maybe Square]]
    getMoveSquares b sq = case getPiece b sq of
        Nothing -> []
        Just (Piece c Pawn) -> map (map (removeCaptureSquare b c)) (pieceMoves sq (Piece c Pawn)) ++ pawnCaptures b c sq
        Just p -> pieceMoves sq p

    removeCaptureSquare :: Board -> Color -> Maybe Square -> Maybe Square
    removeCaptureSquare b c sq = sqToPos b sq >>= \pos -> if canCapture c pos then Nothing else sq

    addCaptureSquare :: Board -> Color -> Maybe Square -> Maybe Square
    addCaptureSquare b c sq = sqToPos b sq >>= \pos -> if canCapture c pos then sq else Nothing

    pawnCaptures :: Board -> Color -> Square -> [[Maybe Square]]
    pawnCaptures b c sq = [[captures (1, if c == White then 1 else -1)], [captures (-1, if c == White then 1 else -1)]]
        where captures xy = moveXY sq xy >>= \sq2 -> addCaptureSquare b c (Just sq2)

    getPositionsFromNext :: Board -> Move -> [[Position]]
    getPositionsFromNext b m = do 
        let (b2, pos) = forwardMove b m
        possiblePositions b2 pos

    forwardMove :: Board -> Move -> (Board, Position)
    forwardMove b m = (addMove b m, Pos (square (endPos m)) (piece (startPos m)))

    isCheckMateMove :: Board -> Color -> Move -> Bool
    isCheckMateMove b c m = checkmate (addMove b m) (opponent c) m

    isCheckMove :: Board -> Color -> Move -> Bool
    isCheckMove b c m = or $ getPositionsFromNext b m >>= \ps -> parsePositionsF ps (parseMove (startPos m) c) (return . any (\m2 -> mtype m2 == Check))

    moveSet :: Board -> Position -> Color -> [Position] -> [Move]
    moveSet b pos c ps = map (swapChecks b c) $ parsePositions ps (parseMove pos c)

    swapChecks :: Board -> Color -> Move -> Move
    swapChecks b c m = if isCheckMove b c m
        then if isCheckMateMove b c m
            then swapType m Checkmate
            else swapType m Check
        else m

    parseBoard :: Board -> Parser a -> [a]
    parseBoard b = parsePositions (positions b)

    parseBoardMaybe :: Board -> Parser (Maybe a) -> [a]
    parseBoardMaybe b p = catMaybes $ parseBoard b p

    getColorPieces :: Board -> Color -> [Position]
    getColorPieces b c = parseBoardMaybe b (allPieces c)
    
    getKingPosition :: Board -> Color -> Maybe Position
    getKingPosition b c = safeHead $ parseBoardMaybe b (findPiece (Piece c King))

    getColorMoves :: Board -> Color -> [[Move]]
    getColorMoves b c = removeEmpties $ getColorPieces b c >>= \(Pos sq _) -> return $ generateLegalMoves b c sq

    getColorMovesList :: Board -> Color -> [Move]
    getColorMovesList b c = concat (getColorMoves b c)
    
    getColorPositions :: Board -> Color -> [[Position]]
    getColorPositions b c = removeEmpties $ getColorPieces b c >>= \(Pos sq _) -> return $ map endPos (generateLegalMoves b c sq)

    getAttackLanes :: Board -> Color -> [[Position]]
    getAttackLanes b c = removeEmpties $ getColorPieces b c >>= possiblePositions b >>= \ps -> return $ parsePositions ps Parser.empty

    movesOutOfCheck :: Board -> Color -> Maybe [Move]
    movesOutOfCheck b c =  getKingPosition b c >>= \(Pos sq _) -> Just $ filterByPos (concat (getAttackLanes b c)) notSqInPos (generateLegalMoves b c sq)

    getCheckLane :: Board -> Color -> Move -> [[Position]]
    getCheckLane b c m = getPositionsFromNext b m >>=
        \case
            [] -> return []
            xs -> case parse (many (movable c)) xs of
                Nothing -> []
                Just ([], _) -> return []
                Just (ys, _) -> case last ys of
                    (Pos _ (Just (Piece col King))) -> if c /= col then return (endPos m : init ys) else return []
                    _ -> return []

    blockCheckMoves :: Board -> Color -> Move -> [Move]
    blockCheckMoves b c m = filterByPos (concat (getCheckLane b (opponent c) m)) sqInPos (removePieceMove King (concat (getColorMoves b c)))
    
    endCheckMoves :: Move -> Board -> Color -> [Move]
    endCheckMoves m b c = blockCheckMoves b c m ++ fromMaybe [] (movesOutOfCheck b c)

    checkmate :: Board -> Color -> Move -> Bool
    checkmate b c m = null $ endCheckMoves m b c

    addMoves :: Board -> [Move] -> Board
    addMoves b [] = b
    addMoves b (m:ms) = addMove (addMoves b ms) m

    testBoard :: Board
    testBoard = addMoves startBoard testMoves 