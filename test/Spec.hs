import Test.HUnit

import SquareTest(squareTests)

main :: IO ()
main = do
    results <- runTestTT $ test squareTests
    if errors results + failures results == 0 then
        putStrLn "Tests passed."
    else
        putStrLn "Tests failed."
