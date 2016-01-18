
import Control.Monad
import Data.List
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
data Board = Board (Set.Set Position) (Set.Set Covered) deriving (Eq)

-- | Gives the width and height of a game board.
boardEdge :: Int
boardEdge = 8

instance Show Board where
  show (Board positions _) =
    let isSet col row = Set.member (positionAt col row) positions
        showCell True = "X"
        showCell False = "-"
        isColSet = isSet <$> [0 .. (boardEdge - 1)]
        boardInfo = (\r -> isColSet <*> [r]) <$> [0 .. (boardEdge - 1)]
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
coveredBy :: Position -> Set.Set Covered
coveredBy (Position i) =
  let (row, col) = i `divMod` boardEdge
      asc = row - col
      dsc = row + col
  in Set.fromList [ Covered DRow row
                  , Covered DCol col
                  , Covered DAsc asc
                  , Covered DDsc dsc ]

-- | Try to add a unique element to a list
tryAddUnique :: (Ord a) => a -> Set.Set a -> Maybe (Set.Set a)
tryAddUnique value existing
  | Set.member value existing = Nothing
  | otherwise = Just $ Set.insert value existing

-- | Take a position on the board, if available
takePosition :: Board -> Position -> Maybe Board
takePosition (Board positions assignments) position =
  let neededAssignments = coveredBy position
      -- Add a unique item to a list.  Fails if the list is Nothing or if the item is not unique
      maybeAddUniqueItem maybeList newItem = maybeList >>= tryAddUnique newItem
  in Board (Set.insert position positions) <$> foldl maybeAddUniqueItem (Just assignments) neededAssignments

pick :: (a -> Maybe b) -> [a] -> Maybe b
pick f [] = Nothing
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
