{-# LANGUAGE LambdaCase #-}

module BoardParser where
    import Piece
    import Control.Applicative

    newtype BoardParser a = BP { parse :: String -> Maybe (a, String)}


    instance Applicative BoardParser where
        pure x = BP $ \b -> Just (x, b)
        (<*>) x y = BP $ \p -> do
            (a, as) <- parse x p
            (b, bs) <- parse y as
            return (a b, bs)

    instance Functor BoardParser where
        fmap f g = BP $ \b -> do
            (p, ps) <- parse g b
            return (f p, ps)

    instance Alternative BoardParser where
        empty = BP $ const Nothing
        (<|>) x y =  BP $ \b -> f (parse x b) (parse y b)
            where   f (Just a) _ = Just a
                    f Nothing a = a

    get :: BoardParser Char
    get = BP $ \case
        (c:cs) -> Just (c, cs)
        [] -> Nothing


    piece :: Piece -> BoardParser Char
    piece p = BP $ \case
        (c:cs) -> if c == '1' then Just (pieceChar p, cs) else Nothing
        [] -> Nothing

    parseForward :: (Piece, String) -> (Piece, String)
    parseForward (p, []) = (p, [])
    parseForward (p, _:cs) = (p, cs)


    
