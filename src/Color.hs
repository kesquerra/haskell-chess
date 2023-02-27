{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Color where

    data Color = Black | White
        deriving (Eq, Ord, Show)

    opponent :: Color -> Color
    opponent White = Black
    opponent Black = White