{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Player where
    import Move
    import Color

    data Player = Player {
        moves :: [Move],
        color :: Color
    } deriving (Show)


    stringMoves :: Player -> [String]
    stringMoves p = map show (moves p)