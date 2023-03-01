module NewBoard where
    import Bitboard
    import Color
    import Piece
    import BoardParser
    import qualified Data.Map as M
    import Data.Bits
    import Square
    
    data Board = Board {
        white :: Bitboard,
        black :: Bitboard,
        occupied :: Bitboard,
        pieces :: M.Map PieceType Bitboard
    } deriving Show

    empty :: Board
    empty = Board Bitboard.empty Bitboard.empty Bitboard.empty M.empty

    start :: Board
    start = do 
        let black = colorPositions Black
        let white = colorPositions White
        Board black white (black <> white) pieceMap

    color :: Board -> Color -> Bitboard
    color b White = white b
    color b Black = black b

    getPieceTypeBitboard :: Board -> PieceType -> Bitboard
    getPieceTypeBitboard b t = pieces b M.! t

    getPieceBitboard :: Board -> Piece -> Bitboard
    getPieceBitboard b (Piece c t) = getPieceTypeBitboard b t .&. color b c
    
    addMove :: Board -> Piece -> Square -> Square -> Board
    addMove (Board w b o p) (Piece White t) sq1 sq2 = Board (moveSquare w sq1 sq2) b (moveSquare o sq1 sq2) (M.insert t (moveSquare (p M.! t) sq1 sq2) p)
    addMove (Board w b o p) (Piece Black t) sq1 sq2 = Board w (moveSquare b sq1 sq2) (moveSquare o sq1 sq2) (M.insert t (moveSquare (p M.! t) sq1 sq2) p)
    
    blocks :: Board -> Piece -> Bitboard.Direction -> Bitboard
    blocks b (Piece c t) d = do
        let pcb = getPieceBitboard b (Piece c t)
        (fill d 7 (color b c .&. complement pcb) .|. pcb) .|. fill d 7 (move d 1 (color b (opponent c)))

    slidingMove :: Board -> Piece -> Bitboard.Direction -> Bitboard
    slidingMove b (Piece c t) d = do
        let pcb =  getPieceBitboard b (Piece c t)
        let att = fill d 7 pcb `xor` pcb
        let blocked = blocks b (Piece c t) d
        att .&. complement blocked

    slidingMoves :: Board -> Piece -> [Bitboard.Direction] -> Bitboard
    slidingMoves b p = foldr ((.|.) . \d -> slidingMove b p d) (getPieceBitboard b p)
    

    pieceMoves :: Board -> Piece -> Bitboard
    pieceMoves b (Piece c Rook) = slidingMoves b (Piece c Rook) [Bitboard.N, Bitboard.S, Bitboard.E, Bitboard.W]
    pieceMoves b (Piece c Bishop) = slidingMoves b (Piece c Rook) [Bitboard.NE, Bitboard.SW, Bitboard.SE, Bitboard.NW]
    pieceMoves b (Piece c Queen) = NewBoard.pieceMoves b (Piece c Rook) <>  NewBoard.pieceMoves b (Piece c Bishop)
    pieceMoves b (Piece c King) = fillMany 1 (getPieceBitboard b (Piece c King)) Bitboard.allDirs
    pieceMoves b (Piece c Knight) = foldr ((.|.) . (\((d1, x1), (d2, x2)) -> move d2 x2 (move d1 x1 (getPieceBitboard b (Piece c Knight))))) (getPieceBitboard b (Piece c Knight)) knightMoves
    pieceMoves b (Piece White Pawn)  = move Bitboard.S 1 (getPieceBitboard b (Piece White Pawn)) <> move Bitboard.S 2 (Bb (0xff `shiftL` 48) .&. getPieceBitboard b (Piece White Pawn))
    pieceMoves b (Piece Black Pawn)  = move Bitboard.N 1 (getPieceBitboard b (Piece Black Pawn)) <> move Bitboard.N 2 (Bb (0xff `shiftL` 8) .&. getPieceBitboard b (Piece Black Pawn))

    testBoard :: Board
    testBoard = addMove (addMove (addMove (addMove start (Piece Black Pawn) (Sq 8 2) (Sq 8 4)) (Piece Black Knight) (Sq 7 1) (Sq 6 3)) (Piece Black Pawn) (Sq 1 2) (Sq 1 3)) (Piece White Knight) (Sq 1 2) (Sq 8 3)