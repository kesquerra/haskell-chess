{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Square where 
    import Convert
    import Piece

    data Square = Sq {
        file :: Int,
        rank :: Int
    } deriving (Eq, Ord)

    data Direction = N | S | E | W | NE | NW | SE | SW

    instance Show Square where
        show = toString

    fromString :: String -> Maybe Square
    fromString [f, r] = fileCharToInt f >>= \fi -> charToInt r >>= \ri -> Just $ Sq fi ri
    fromString _ = Nothing

    toString :: Square -> String
    toString sq = intToChar (file sq) : show (rank sq)

    index :: Square -> Int
    index sq = (rank  sq-1) * 8 + (file sq-1)

    moveX :: Square -> Int -> Maybe Square
    moveX sq x = if isValidIndex $ file sq + x
        then Just $ Sq (file sq + x) $ rank sq
        else Nothing

    moveY :: Square -> Int -> Maybe Square
    moveY sq y = if isValidIndex $ rank sq + y
        then Just $ Sq (file sq) $ rank sq + y
        else Nothing

    moveXY :: Square -> (Int, Int) -> Maybe Square
    moveXY sq (x, y) = moveX sq x >>= \sq2 -> moveY sq2 y

    moveXYs :: Square -> [(Int, Int)] -> [Maybe Square]
    moveXYs sq xys = xys >>= \xy -> return $ moveXY sq xy

    generateRank :: Int -> [Square]
    generateRank i = [Sq x i | x <- [1..8]]

    generateFile :: Int -> [Square]
    generateFile i = [Sq i x | x <- [1..8]]

    rangeDir :: Direction -> Square -> Int -> [Maybe Square]
    rangeDir N sq i = [moveY sq x | x <- [1..i]]
    rangeDir S sq i = [moveY sq (-x) | x <- [1..i]]
    rangeDir E sq i = [moveX sq x | x <- [1..i]]
    rangeDir W sq i = [moveX sq (-x) | x <- [1..i]]
    rangeDir SE sq i = [moveXY sq (x, -x) | x <- [1..i]]
    rangeDir SW sq i = [moveXY sq (-x, -x) | x <- [1..i]]
    rangeDir NE sq i = [moveXY sq (x, x) | x <- [1..i]]
    rangeDir NW sq i = [moveXY sq (-x, x) | x <- [1..i]]

    fullRangeDir :: Direction -> Square -> [Maybe Square]
    fullRangeDir N sq = rangeDir N sq (8 - rank sq)
    fullRangeDir S sq = rangeDir S sq (rank sq - 1) 
    fullRangeDir E sq = rangeDir E sq (8 - file sq) 
    fullRangeDir W sq = rangeDir W sq (file sq - 1)
    fullRangeDir NE sq = rangeDir NE sq $ (8 - file sq) `max` (8 - rank sq)
    fullRangeDir NW sq = rangeDir NW sq $ (8 - rank sq) `max` (file sq - 1)
    fullRangeDir SE sq = rangeDir SE sq $ (rank sq - 1) `max` (8 - file sq) 
    fullRangeDir SW sq = rangeDir SW sq $ (file sq - 1) `max` (rank sq -1)

    allDirs :: [Direction]
    allDirs = [N, NE, NW, S, SE, SW, E, W]

    pieceMoves :: Square -> PieceType -> [[Maybe Square]]
    pieceMoves sq Rook = [fullRangeDir x sq | x <- [N, S, E, W]]
    pieceMoves sq King = [rangeDir x sq 1 | x <- allDirs]
    pieceMoves sq Knight = [moveXYs sq [(x, y)] | x <- [-1, 1, 2, -2], y <- [-1, 1, 2, -2], abs x /= abs y]
    pieceMoves sq Bishop = [fullRangeDir x sq | x <- [NE, SE, NW, SW]]
    pieceMoves sq Queen = [fullRangeDir x sq | x <- allDirs]
    pieceMoves sq Pawn = [rangeDir N sq 1]
