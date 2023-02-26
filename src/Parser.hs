{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}
module Parser where 
    import Control.Monad(guard)
    import Control.Applicative
    import Data.Maybe

    import Position
    import Color
    import Move
    import Piece


    newtype Parser a = P { parse :: [Position] -> Maybe (a, [Position])}

    instance Monad Parser where
        (>>=) p f = P $ \b -> do
            (a, _) <- parse p b
            (m, ms) <- parse (f a) b
            return (m, ms)

    instance Functor Parser where
        fmap f g = P $ \b -> do
            (p, ps) <- parse g b
            return (f p, ps)

    instance Applicative Parser where
        pure x = P $ \b -> Just (x, b)
        (<*>) x y = P $ \p -> do
            (a, as) <- parse x p
            (b, bs) <- parse y as
            return (a b, bs)  

    instance Alternative Parser where
        empty = P $ const Nothing
        (<|>) x y = P $ \b -> f (parse x b) (parse y b)
            where   f (Just a) _ = Just a
                    f Nothing a = a

    get :: Parser Position
    get = P $ \case
        (p:ps) -> Just (p, ps)
        [] -> Nothing

    satisfy :: (Position -> Bool) -> Parser Position
    satisfy f = Parser.filter f get

    filter :: (a -> Bool) -> Parser a -> Parser a
    filter f g = P $ \b -> do
        (p, ps) <- parse g b
        guard (f p)
        return (p , ps)

    empty :: Parser Position
    empty = satisfy (\(Pos _ p) -> isNothing p)

    capture :: Color -> Parser Position
    capture c = satisfy (canCapture c)

    movable :: Color -> Parser Position
    movable c = Parser.empty <|> capture c

    moveGeneric :: Position -> (Bool, Bool, Bool, Bool) -> Parser Move
    moveGeneric p1 (a, b, c, d) = P $ \case
        (p:ps) -> Just (M p1 p a b c d, ps)
        [] -> Nothing

    checkMove :: Position -> Color -> Parser (Maybe Move)
    checkMove p1 c = movable c >> P (\case
        (p:ps) -> case p of
            (Pos _ (Just (Piece col King))) -> if col /= c then Just (Just (M p1 p True False False False), ps) else Nothing
            (Pos _ Nothing) -> Just (Nothing, ps)
            (Pos _ _) -> Nothing
        [] -> Nothing)

    getMove :: Position -> Parser Move
    getMove p = moveGeneric p (False, False, False, False)  

    move :: Position -> Parser Move
    move p = Parser.empty >> getMove p

    captureMove :: Position -> Color -> Parser Move
    captureMove p1 c = capture c >> P (\case
        (p:_) -> Just (M p1 p False False False False, [])
        [] -> Nothing)

    findKing :: Color -> Parser (Maybe Position)
    findKing c = P $ \case
        ((Pos sq (Just (Piece col King))):ps) -> if col == c then Just (Just (Pos sq (Just (Piece col King))), []) else Just (Nothing, ps)
        (_:ps) -> Just (Nothing, ps)
        _ -> Nothing


    