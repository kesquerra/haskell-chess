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

    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x:_) = Just x

    safeSwap :: [Maybe a] -> a -> a
    safeSwap [] x = x
    safeSwap (x:xs) y = case x of
        Nothing -> safeSwap xs y
        Just v -> v

    defaultSwap :: b -> Maybe b -> b
    defaultSwap b Nothing = b
    defaultSwap _ (Just b) = b

    swapF :: (a -> b -> c -> [c]) -> a -> b -> c -> c
    swapF f b c m = defaultSwap m (safeHead (f b c m))

    removeEmpties :: [[a]] -> [[a]]
    removeEmpties [] = []
    removeEmpties (a:as) = case a of
        [] -> removeEmpties as
        (b:bs) -> (b:bs) : removeEmpties as

    contains :: Eq a => a -> [a] -> Bool
    contains _ [] = False
    contains x xs = x `elem` xs