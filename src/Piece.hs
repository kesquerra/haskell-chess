{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Piece where

    import Data.Char(toUpper)
    import Color

    data PieceType = King | Queen | Rook | Bishop | Knight | Pawn
        deriving (Eq)

    data Piece = Piece Color PieceType
        deriving (Eq, Ord)

    instance Show PieceType where
        show t = case t of
                    King -> "k"
                    Queen -> "q"
                    Bishop -> "b"
                    Knight -> "n"
                    Rook -> "r"
                    Pawn -> "p"

    instance Ord PieceType where
        (<=) x y = pieceValue x <= pieceValue y

    instance Show Piece where
        show (Piece White t) = map toUpper $ show t
        show (Piece Black t) = show t

    pieceValue :: PieceType -> Int
    pieceValue King = 0
    pieceValue Queen = 8
    pieceValue Rook = 5
    pieceValue Pawn = 1
    pieceValue _ = 3

    backRankPieces :: Color -> [Piece]
    backRankPieces c = [ Piece c Rook, Piece c Knight, Piece c Bishop,
                         Piece c Queen, Piece c King, Piece c Bishop,
                         Piece c Knight, Piece c Rook
                        ]

    frontRankPieces :: Color -> [Piece]
    frontRankPieces c = replicate 8 $ Piece c Pawn

    isPieceType :: Maybe Piece -> PieceType -> Bool
    isPieceType Nothing _ = False
    isPieceType (Just (Piece _ t1)) t2 = t1 == t2   

    printMaybePiece :: Maybe Piece -> String
    printMaybePiece Nothing = "_"
    printMaybePiece (Just p) = show p