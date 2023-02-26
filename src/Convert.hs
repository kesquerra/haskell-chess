{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Convert where
    import Data.Char(ord, chr)

    isValidIndex :: Int -> Bool
    isValidIndex i = i >= 1 && i <= 8

    charToInt :: Char -> Maybe Int
    charToInt c
        | ord c < 49 || ord c > 56 = Nothing
        | otherwise = Just $ ord c - 48

    fileCharToInt :: Char -> Maybe Int
    fileCharToInt c
        | ord c < 97 || ord c > 104 = Nothing
        | otherwise = Just $ ord c - 96

    intToChar :: Int -> Char
    intToChar i = chr (i + 96)
