{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Player where
    import Move
    import Color
    import Data.List(sort)

    data Player = Player {
        moves :: [Move],
        color :: Color
    } deriving (Show)


    stringMoves :: Player -> [[String]]
    stringMoves p = splitMoves $ map show (sort (moves p))

    splitMoves :: [String] -> [[String]]
    splitMoves [] = [[]]
    splitMoves xs = if length xs >= 6 then take 6 xs : splitMoves (drop 6 xs) else [xs]