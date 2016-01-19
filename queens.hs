
import Control.Monad
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

{-| One dimension by which the board can be covered -}
data Dimension = DRow -- horizontal row
               | DCol -- vertical col
               | DAsc -- L->R, B->T
               | DDsc -- L->R, T->B
               deriving (Eq, Ord)

{-| A "Covered" area of hte board into which no Queen should be placed.
    Each queen covers a row, column, ascender, and descender - represented by the dimension and a
    number (row number, column number, etc).
 -}
data Covered = Covered Dimension Int deriving (Eq, Ord)

{-| A position on the board.  Tiles are counted from 0 starting at the top-left, incrementing by 1
    across (within a row) and then down (into a new row).
-}
newtype Position = Position Int deriving (Eq, Ord)

{-| A game board containing a set of placed queens and tracked data indicating what areas are covered.
 -}
data Board = Board (Set Position) (Set Covered) deriving (Eq)

-- | Gives the width and height of a game board.
boardEdge :: Int
boardEdge = 8

instance Show Board where
  show (Board positions _) =
    let edgeIndices = [0 .. (boardEdge - 1)]
        isSet col row = Set.member (positionAt col row) positions
        boardInfo = [ [ isSet col row | col <- edgeIndices ] | row <- edgeIndices ]
        showCell True = "X"
        showCell False = "-"
        showRow tiles = join (showCell <$> tiles)
    in
    intercalate "\r\n" (showRow <$> boardInfo)

-- | List of all possible positions on a board
allPositions :: [Position]
allPositions = Position <$> [ 0.. (boardEdge * boardEdge - 1)]

positionAt :: Int -> Int -> Position
positionAt col row = Position $ row * boardEdge + col

emptyBoard :: Board
emptyBoard = Board Set.empty Set.empty

isSolved :: Board -> Bool
isSolved (Board positions _) = length positions == boardEdge

{- | Gets the coverage a position has.  Used to determine what row,col,asc,dsc will be occupied
     if a queen is placed in the given position. -}
coveredBy :: Position -> Set Covered
coveredBy (Position i) =
  let (row, col) = i `divMod` boardEdge
      asc = row - col
      dsc = row + col
  in Set.fromList [ Covered DRow row
                  , Covered DCol col
                  , Covered DAsc asc
                  , Covered DDsc dsc ]

{- | Attempt to take the given position.  Fails (Nothing) if any Queen on the board already covers
     the position.  Returns the new Board (if possible) with new coverage state. -}
takePosition :: Board -> Position -> Maybe Board
takePosition (Board positions covered) position
  | any (`Set.member` covered) positionCovers = Nothing
  | otherwise = Just $ Board combinedPositions combinedCovered
  where positionCovers = coveredBy position
        combinedPositions = Set.insert position positions
        combinedCovered = Set.union covered positionCovers

pick :: (a -> Maybe b) -> [a] -> Maybe b
pick _ [] = Nothing
pick f (head : tail) =
  case f head of
  Nothing -> pick f tail
  justx -> justx

-- | Solve the given board, placing queens as needed.
solve :: Board -> Maybe Board
solve board
  | isSolved board = Just board
  | otherwise = pick (takePosition board >=> solve) allPositions

main =
  case solve emptyBoard of
    Nothing -> putStrLn "No solution"
    Just board -> print board
