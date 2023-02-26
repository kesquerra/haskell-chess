{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Art
    where

        horizontalLine :: String
        horizontalLine = "  -----------------------------------------\n"

        fileLabels :: String
        fileLabels = concat [x : "    " | x <- ['a'..'h']] ++ "\n\n"

        tab :: String
        tab = "  "

        emptyLineWithBorder :: String
        emptyLineWithBorder = "  |\t\t\t\t          |\n"


        banner :: String
        banner = "          888                              \n          888                              \n          888                              \n   .d8888b88888b.  .d88b. .d8888b .d8888b  \n  d88P\"   888 \"88bd8P  Y8b88K     88K      \n  888     888  88888888888\"Y8888b.\"Y8888b. \n  Y88b.   888  888Y8b.         X88     X88 \n   \"Y8888P888  888 \"Y8888  88888P' 88888P' "