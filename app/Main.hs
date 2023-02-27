module Main (main) where
import Game(Game(..), newGame, playerTurn, checkmate, getState, changeState)
import State(State(..))


main :: IO ()
main = do
    let g = newGame
    final <- turnLoop g
    print final



turnLoop :: Game -> IO Game
turnLoop g = do
    print g
    new_g <- playerTurn g
    result <- getState new_g
    case result of
        Checkmate -> return new_g 
        _ -> turnLoop new_g

