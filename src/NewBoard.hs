module NewBoard where
    import Bitboard
    import Color
    import Piece
    import qualified Data.Map as M
    import Data.Bits
    import Square
    import BoardParser
    import Control.Applicative
    
    data Board = Board {
        white :: Bitboard,
        black :: Bitboard,
        occupied :: Bitboard,
        pieces :: M.Map PieceType Bitboard
    }

    instance Show Board where
        show b = parseSet (stringSet b White)

    parseSet :: [(Piece, String)] -> String
    parseSet xs = case foldr ((<|>) . (\(p, s) -> parse (piece p) s)) Nothing xs of
        Nothing -> linebreak xs : '.' : parseSet (map parseForward xs)
        Just (c, cs) -> linebreak xs : if null cs then c : "" else c : parseSet (map parseForward xs)

    linebreak :: [(Piece, String)] -> Char
    linebreak [] = '\n'
    linebreak ((_, s):_) = if length s `mod` 8 == 0 then '\n' else '\00'

    stringSet :: Board -> Color -> [(Piece, String)]
    stringSet b c = map (\(t, bb) -> ((Piece c t), Bitboard.toString bb)) (M.toList (pieces b))
    
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
        let pcb =  getPieceBitboard b (Piece c t)       -- piece specific bitboard
        let att = fill d 7 pcb `xor` pcb                -- attack squares for piece (without the piece itself)
        let blocked = blocks b (Piece c t) d            -- all blocked squares (inluding own pieces)
        att .&. complement blocked                      -- all attack squares that are not blocked

    slidingMoves :: Board -> Piece -> [Bitboard.Direction] -> Bitboard
    slidingMoves b p = foldr ((.|.) . \d -> slidingMove b p d) (getPieceBitboard b p)

    pieceMoves :: Board -> Piece -> Bitboard
    pieceMoves b (Piece c Rook)     = slidingMoves b (Piece c Rook) [Bitboard.N, Bitboard.S, Bitboard.E, Bitboard.W]
    pieceMoves b (Piece c Bishop)   = slidingMoves b (Piece c Rook) [Bitboard.NE, Bitboard.SW, Bitboard.SE, Bitboard.NW]
    pieceMoves b (Piece c Queen)    = NewBoard.pieceMoves b (Piece c Rook) <>  NewBoard.pieceMoves b (Piece c Bishop)
    pieceMoves b (Piece c King)     = fillMany 1 (getPieceBitboard b (Piece c King)) Bitboard.allDirs
    pieceMoves b (Piece c Knight)   = knightMoves (getPieceBitboard b (Piece c Knight))
    pieceMoves b (Piece c Pawn)     = pawnMoves (getPieceBitboard b (Piece c Pawn)) (Piece c Pawn)

    testBoard :: Board
    testBoard = addMove (addMove (addMove (addMove start (Piece Black Pawn) (Sq 8 2) (Sq 8 4)) (Piece Black Knight) (Sq 7 1) (Sq 6 3)) (Piece Black Pawn) (Sq 1 2) (Sq 1 3)) (Piece White Knight) (Sq 1 2) (Sq 8 3)