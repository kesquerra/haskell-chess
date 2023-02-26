{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Move where
    import Position
    import Piece

    data Move = M {
        startPos :: Position,
        endPos :: Position,
        isCheck :: Bool,
        isCheckmate :: Bool,
        isKCastle :: Bool,
        isQCastle :: Bool
    }

    instance Show Move where
        show = moveString
    
    moveString :: Move -> String
    moveString (M (Pos _ p1)  (Pos pos2 p2) chk chkm kc qc)
        | kc = "O-O"
        | qc = "O-O-O"
        | otherwise = case p1 of
                        Just (Piece _ Pawn) -> ""
                        Just pc -> show pc
                        _ -> ""
                        ++ case p2 of
                            Nothing -> ""
                            Just _ -> "x"
                        ++ show pos2 ++ if chkm then "#" else if chk then "+" else ""