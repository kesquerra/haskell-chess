module Bitboard where
    import Piece
    import Color
    import Numeric (showIntAtBase)
    import Data.Char (intToDigit)
    import Data.List (intercalate)
    import Data.Bits
    import Data.Word
    import qualified Data.Map as M
    import qualified Square as S

    -- Based on ideas presented at https://www.chessprogramming.org/General_Setwise_Operations#ShiftingBitboards

    data Direction = N | S | E | W | NE | NW | SE | SW
        deriving Show

    newtype Bitboard = Bb Word64
        deriving (Eq)

    instance Bits Bitboard where
        (Bb v1) .&. (Bb v2) = Bb (v1 .&. v2)
        (Bb v1) .|. (Bb v2) = Bb (v1 .|. v2)
        xor (Bb v1) (Bb v2) = Bb (xor v1 v2)
        complement (Bb v) = Bb (complement v)
        shift (Bb v) i = Bb (shift v i)
        rotate (Bb v) i = Bb $ rotate v i
        bitSizeMaybe (Bb v) = bitSizeMaybe v
        isSigned (Bb v) = isSigned v
        testBit (Bb v)= testBit v
        bit i = Bb $ bit i
        popCount (Bb v) = popCount v
        bitSize (Bb v) = bitSize v

    instance Show Bitboard where
        show bb =  intercalate "\n" $ splitString (toString bb)

    instance Semigroup Bitboard where
        (<>) = (.|.)

    instance Monoid Bitboard where
        mempty = empty

    padZeros :: String -> String
    padZeros s = if length s == 64 then s else replicate (64-length s) '0' ++ s

    splitString :: String -> [String]
    splitString [] = []
    splitString xs = take 8 xs : splitString (drop 8 xs)

    toString :: Bitboard -> String
    toString (Bb i) = padZeros (showIntAtBase 2 intToDigit i "")

    empty :: Bitboard
    empty = Bb 0

    moveBit :: Bitboard -> Int -> Int -> Bitboard
    moveBit b a = setBit (clearBit b a)

    moveSquare :: Bitboard -> S.Square -> S.Square -> Bitboard
    moveSquare b sq1 sq2 = moveBit b (S.index sq1) (S.index sq2)

    -- hex values that represent the starting piece positions when converted to binary
    piecePositions :: PieceType -> Bitboard
    piecePositions Pawn     = Bb 0xff00000000ff00
    piecePositions Knight   = Bb 0x4200000000000042
    piecePositions Bishop   = Bb 0x2400000000000024
    piecePositions Rook     = Bb 0x8100000000000081
    piecePositions Queen    = Bb 0x800000000000008
    piecePositions King     = Bb 0x1000000000000010

    colorPositions :: Color -> Bitboard
    colorPositions White = Bb 0xffff
    colorPositions Black = Bb 0xffff000000000000

    pieceMap :: M.Map PieceType Bitboard
    pieceMap = M.fromList $ map (\t -> (t, piecePositions t)) [Pawn, Knight, Bishop, Rook, Queen, King]

    testPieceMap :: M.Map PieceType Bitboard
    testPieceMap = M.fromList $ map (\t -> (t, piecePositions t)) [Knight, Bishop, Rook, Queen, King]

    
    move :: Direction -> Int -> Bitboard -> Bitboard
    move _ 0 bb = bb
    move N i bb = move N (i-1) bb `shiftL` 8 .&. complement (Bb 0xff)                                       -- remove the first rank on wraparound
    move S i bb = move S (i-1) bb `shiftR` 8 .&. complement (Bb 0xff `shiftL` 56)                           -- remove the eigth rank on wraparound
    move W i bb = move W (i-1) ((bb `shiftR` 1) .&. Bb 0x7f7f7f7f7f7f7f7f)                                  -- hex value removes the h file due to wraparound shift
    move E i bb = move E (i-1) ((bb `shiftL` 1) .&. Bb 0xfefefefefefefefe)                                  -- hex value removes the a file due to wraparound shift
    move NW i bb = move W i (move N i bb)
    move SW i bb = move W i (move S i bb)
    move NE i bb = move E i (move N i bb)
    move SE i bb = move E i (move S i bb)

    knightPairs :: [((Direction, Int), (Direction, Int))]
    knightPairs = [((d1, x1), (d2, x2)) | d1 <- [N, S], d2 <- [E,W], x1 <- [2, 1], x2 <- [2,1], x1 /= x2]   -- (direction, amt) patterns for knight move 

    knightMoves :: Bitboard -> Bitboard
    knightMoves b = foldr ((.|.) . (\((d1, x1), (d2, x2)) -> move d2 x2 (move d1 x1 b))) b knightPairs

    pawnMoves :: Bitboard -> Piece -> Bitboard 
    pawnMoves b (Piece White _) = move N 1 b <> move N 2 (Bb (0xff `shiftL` 8) .&. b)                       -- move forward one, if on second rank, move 2
    pawnMoves b (Piece Black _) = move S 1 b <> move S 2 (Bb (0xff `shiftL` 56) .&. b)                      -- move forward one, if on seventh rank, move 2
    
    fill :: Direction -> Int -> Bitboard -> Bitboard
    fill dir i pcb = foldr ((.|.) . (\x -> move dir x pcb)) pcb [1..i]

    fillMany :: Int -> Bitboard -> [Direction] -> Bitboard
    fillMany i pcb = foldr ((.|.) . (\x -> fill x i pcb)) empty

    allDirs :: [Direction]
    allDirs = [NE, SE, NW, SW, N, S, E, W]