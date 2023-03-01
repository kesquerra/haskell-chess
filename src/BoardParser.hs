{-# LANGUAGE LambdaCase #-}

module BoardParser where
    import Piece

    newtype BoardParser a = BP { parse :: String -> Maybe (a, String)}

    get :: BoardParser Char
    get = BP $ \case
        (c:cs) -> Just (c, cs)
        [] -> Nothing


    piece :: Piece -> BoardParser Char
    piece p = BP $ \case
        (c:cs) -> if c == '1' then Just (pieceChar p, cs) else Nothing
        [] -> Nothing
