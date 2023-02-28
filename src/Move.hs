{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Move where
    import Position
    import Piece
    import Color
    import Square
    import Data.Maybe
    import Data.String

    data Move = M {
        startPos :: Position,
        endPos :: Position,
        mtype :: MoveType
    } deriving (Eq)

    data MoveType = Check | Checkmate | KingsideCastle | QueensideCastle | Capture | Move | CaptureCheck
        deriving (Eq)

    instance Ord MoveType where
        compare x y = if x == y then EQ else
            case (x, y) of
                (Checkmate, _) -> GT
                (CaptureCheck, _) -> GT
                (Check, _) -> GT
                (Capture, _) -> GT
                (KingsideCastle, _) -> EQ
                (QueensideCastle, _) -> EQ
                (Move, _) -> EQ
    instance Show Move where
        show = moveString
    
    instance Ord Move where
        (<=) x y = piece (startPos x) <= piece (startPos y) 

    moveString :: Move -> String
    moveString (M (Pos sq p1)  (Pos pos2 p2) mt)
        | mt == KingsideCastle = "O-O"
        | mt == QueensideCastle = "O-O-O"
        | otherwise = case p1 of
                        Just (Piece _ Pawn) -> if isNothing p2 then "" else fileString sq
                        Just (Piece _ t) -> show (Piece White t)
                        _ -> ""
                        ++ case p2 of
                            Nothing -> ""
                            Just _ -> "x"
                        ++ show pos2 ++ if mt == Checkmate then "#" else if mt == Check || mt == CaptureCheck then "+" else ""

    removePieceMove :: PieceType -> [Move] -> [Move]
    removePieceMove _ [] = []
    removePieceMove t (m:ms) = case piece (startPos m) of
        Nothing -> removePieceMove t ms
        (Just (Piece _ ty)) -> if t == ty then removePieceMove t ms else m : removePieceMove t ms

    moveFromString :: String -> [Move] -> Maybe Move
    moveFromString _ [] = Nothing
    moveFromString s (m:ms) = if s == show m then Just m else moveFromString s ms

    swapType :: Move -> MoveType -> Move
    swapType (M p1 p2 _) = M p1 p2

    addCheck :: Move -> Move
    addCheck (M p1 p2 Capture) = M p1 p2 CaptureCheck
    addCheck (M p1 p2 Checkmate) = M p1 p2 Checkmate
    addCheck (M p1 p2 _) = M p1 p2 Check


    testMoves :: [Move]
    testMoves = [M (Pos (Sq 4 1) (Just (Piece White Queen))) (Pos (Sq 3 6) Nothing) Move , M (Pos (Sq 4 7) (Just (Piece Black Pawn))) (Pos (Sq 1 1) Nothing) Move]


    filterByPos :: [Position] -> (Square -> [Position] -> Bool) -> [Move] -> [Move]
    filterByPos ps f = filter (\x -> square (endPos x) `f` ps)

    