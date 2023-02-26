{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Position where
    import Square
    import Piece
    import Color
    import Art

    data Position = Pos Square (Maybe Piece)

    instance Show Position where
        show (Pos sq p) = show sq ++ ": " ++ show p


    piece :: Position -> Maybe Piece
    piece (Pos _ p) = p

    emptyRank :: Int -> [Position]
    emptyRank i = map (`Pos` Nothing) (generateRank i)

    filledRank :: Color -> (Color -> Int) -> (Color -> [Piece]) -> [Position]
    filledRank c r ps = [Pos x y | (x, y) <- zip (generateRank (r c)) (map Just (ps c))]

    backRank :: Color -> [Position]
    backRank c = filledRank c (\x -> if x == White then 1 else 8) backRankPieces

    frontRank :: Color -> [Position]
    frontRank c = filledRank c (\x -> if x == White then 2 else 7) frontRankPieces

    positionString :: Position -> String
    positionString (Pos sq p)
        | file sq == 8  = tab ++ printMaybePiece p ++ tab ++ "|\n" ++ emptyLineWithBorder
        | file sq == 1  = printMaybePiece p ++ tab
        | otherwise     = tab ++ printMaybePiece p ++ tab

    canCapture :: Color -> Position -> Bool
    canCapture c (Pos _ (Just (Piece col _))) = col /= c
    canCapture _ _ = False





    