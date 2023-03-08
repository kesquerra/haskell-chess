## Project Details
---

Name: Chesskell \
Project Members: Kyle Esquerra \
[Github Repo](https://github.com/kesquerra/haskell-chess)

---

## Checkpoint
---

### Game Module
---
Export functions:
- `Game(..)`
  ```haskell
    data Game = G {
        board :: Board,
        moves :: [Move],
        players :: (Player, Player),
        turn :: Color,
        state :: State
    }
    ```
- `newGame`
- `playerTurn`
- `getState`

Depends on:
- `Board`
- `Move`
- `Player`
- `Color`
- `State`


### Board Module
---
Export functions:
- `Board(..)`
    ```haskell
    newtype Board = B {
        positions :: [Position]
    }
    ```
- `startBoard`
- `boardString`
- `addMove`
- `getColorMovesList`
- `endCheckMoves`

Depends on: 
- `Square`
- `Piece`
- `Position`
- `Move`
- `Parser`
- `Color`


### Player Module
---
Export functions:
- `Player(..)`
    ```haskell
    data Player = Player {
        moves :: [Move],
        color :: Color
    }
    ```
- `stringMoves`

Depends on:
- `Move`
- `Color`


### Move Module
---
Export functions:
- `Move(..)`
    ```haskell
    data Move = M {
        startPos :: Position,
        endPos :: Position,
        mtype :: MoveType
    }
    ```
- `MoveType(..)`
    ```haskell
    data MoveType = Check | Checkmate | KingsideCastle | QueensideCastle | Capture | Move | CaptureCheck
    ```
- `moveString`
- `swapType`

Depends on:
- `Piece`
- `Position`
- `Color`
- `Square`

### State Module
---
Export functions:
- `State(..)`
    ```haskell
    data State = Ongoing | Checkmate | Stalemate | Check Move
    ```
- `moveState`

Depends on:
- `Move`

### Parser Module
---
Export functions:
- `Parser(..)`
    ```haskell
    newtype Parser a = P { parse :: [Position] -> Maybe (a, [Position])}
    ```
- `parseMove`
- `allPieces`
- `findPiece`

Depends on:
- `Position`
- `Piece`
- `Move`
- `Color`

### Position Module
---
Export functions:
- `Position(..)`
    ```haskell
    data Position = Pos Square (Maybe Piece)
    ```
- `canCapture`
- `isOpponentKing`
- `sqInPos`
- `notSqInPos`

Depends on:
- `Piece`
- `Square`
- `Color`


### Square Module
---
Export functions:
- `Square(..)`
    ```haskell
    data Square = Sq {
        file :: Int,
        rank :: Int
    }
    ```
- `pieceMoves`
- `moveXY`
- `index`

Depends on:
- `Piece`
- `Color`

### Piece Module
---
Export functions:
- `Piece(..)`
    ```haskell
    data Piece = Piece Color PieceType
    ```
- `PieceType(..)`
    ```haskell
    data PieceType = King | Queen | Rook | Bishop | Knight | Pawn
    ```
- `pieceChar`
- `printMaybePiece`

Depends on:
- `Color`

### Color Module
---
Export functions:
- `Color(..)`
    ```haskell
    data Color = Black | White
    ```
- `opponent`

No dependencies



### Testing
---

I plan to implement testing for board states to determine legal moves available, proper board state (check, checkmate, stalemate, etc.).
With these board states, I will also be able to do testing to ensure that the proper piece is returned from a position, the proper moves are
set for the particular piece, etc.
The next step for this is to create a parser that creates a board from
a [FEN](https://www.chessprogramming.org/Forsyth-Edwards_Notation) string and then be able to run tests on each board position to ensure that all the accurate moves/states/pieces are returning correctly.