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
        (<|>) = por


    por :: Parser a -> Parser a -> Parser a
    por x y = P $ \b -> f (parse x b) (parse y b)
            where   f (Just a) _ = Just a
                    f Nothing a = a

    get :: Parser Position
    get = P $ \case
        (p:ps) -> Just (p, ps)
        [] -> Nothing

    getStop :: Parser Position
    getStop = P $ \case
        (p:_) -> Just (p, [])
        [] -> Nothing

    satisfy :: (Position -> Bool) -> Parser Position
    satisfy f = Parser.filter f get

    satisfyStop :: (Position -> Bool) -> Parser Position
    satisfyStop f = Parser.filter f getStop

    filter :: (a -> Bool) -> Parser a -> Parser a
    filter f g = P $ \b -> do
        (p, ps) <- parse g b
        guard (f p)
        return (p , ps)

    empty :: Parser Position
    empty = satisfy (\(Pos _ p) -> isNothing p)

    capture :: Color -> Parser Position
    capture c = satisfy (canCapture c)

    selfcapture :: Color -> Parser Position
    selfcapture c = satisfyStop (canCapture (opponent c)) <|> Parser.empty

    movable :: Color -> Parser Position
    movable c = capture c <|> Parser.empty

    movableSelfcapture :: Color -> Parser Position
    movableSelfcapture c = selfcapture c <|> Parser.empty

    check :: Color -> Parser Position
    check c = satisfyStop (isOpponentKing c)

    getMove :: Position -> MoveType -> Parser Move
    getMove p1 mt = P $ \case
        (p:ps) -> Just (M p1 p mt, ps)
        [] -> Nothing

    -- parsing positions into moves

    parseMove :: Position -> Color -> Parser Move
    parseMove p1 c1 = P $ \case
        (p2:ps) -> case p2 of
            (Pos _ (Just (Piece c2 King))) -> if c1 /= c2
                then Just (M p1 p2 Check, []) 
                else Nothing
            (Pos _ (Just (Piece c2 _))) -> if c1 /= c2 then Just (M p1 p2 Capture, []) else Nothing
            _ -> Just (M p1 p2 Move, ps)
        _ -> Nothing

    findPiece :: Piece -> Parser (Maybe Position)
    findPiece p1 = P $ \case
        ((Pos sq (Just p2)):ps) -> if p1 == p2 then Just (Just (Pos sq (Just p2)), []) else Just (Nothing, ps)
        (_:ps) -> Just (Nothing, ps)
        _ -> Nothing

    -- get all pieces from the board via parsing

    allPieces :: Color -> Parser (Maybe Position)
    allPieces c = P $ \case
        ((Pos sq (Just (Piece col p))):ps) -> if col == c then Just (Just (Pos sq (Just (Piece col p))), ps) else Just (Nothing, ps)
        ((Pos _ Nothing):ps) -> Just (Nothing, ps)
        _ -> Nothing

    parsePositions :: [Position] -> Parser a -> [a]
    parsePositions ps p = case parse (many p) ps of
        Just (xs, _) -> xs
        Nothing -> []

    parsePositionsF :: [Position] -> Parser a -> ([a] -> [b]) -> [b]
    parsePositionsF ps p f = case parse (many p) ps of
        Just (xs, _) -> f xs
        Nothing -> []
        