# CS557: Functional Programming Project

### Project Details
---

Name: Chesskell \
Project Members: Kyle Esquerra

---

## Chess Representation
---
### Board 
  ```haskell
  [Position]
  ```
  List of 64 positions representing a standard chess board
### Position
  ```haskell
  Square (Maybe Piece)
  ```
  Position on the board denoting the specific square and the piece
  (if one exists) on that square. \
### Square
  ```haskell
  Int Int
  ```
  Square on the board (A1, A2, ..., H7, H8)
### Pieces 
  ```haskell
  Color PieceType
  ```
  Piece representation denoting the color and what type of piece
### PieceType
```haskell
King | Queen | Bishop | Knight | Rook | Pawn
```
Specific chess piece type
### Color
```haskell
Black | White
```
Color denoting the piece color and player that is playing that side

## Game Loop
---

### Game
```haskell
Board [Move] (Player, Player) Color State
```
Game representation that contains the board, the moves played,
both players, turn color, and the game state
### Player
```haskell
[Move] Color
```
Player representation that contains their color being played and their
legal moves
### State
```haskell
Ongoing | Checkmate | Check | Stalemate
```
Game state to determine actions. Checkmate/Statemate end the game.
Check alters the legal moves as the King cannot remain in check.
Ongoing continues play as usual.
### Move
```haskell
(Position, Position)
```
Move representation that contains position moved from and moved to.
### Turn
```haskell
Color
```
Determines the player who's turn it is.

## Processing
---

### Parser
```haskell
[Position] -> Maybe (a, [Position])
```
This parser allows for checking for legal moves in the chess game.
As a player cannot self capture, or move past an occupied square,
this is a great application of a parser to determine the type of move
that can be played.

## Techniques to be Used
---
### Bind
The bind (`>>=`) operation will be a great addition to this project, as
`Position`s contain `Maybe` types, as well as generating possible moves could be done with valid positions returning `Just` values and invalid positions returning `Nothing` values.

### Applicative
The applicative or `<|>` is a great tool as well in the parsing of spaces, as a position could be empty, occupied by own piece, or occupied by an opponent piece. This could allow for separate parsers
to be combined.

### List Comprehension
List comprehensions will be very useful to generate possible move coordinates for `Piece` and `Board`.

### Monoid
`Monoid` will be useful for the `Board` to maintain the `newtype` and 
allow for new instances for `Show`, `Eq`, etc. and being able to retain
the genereal list operations.