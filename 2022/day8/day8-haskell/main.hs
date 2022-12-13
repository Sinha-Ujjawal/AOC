{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.Char (isDigit)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine

data Digit
  = NegNine
  | NegEight
  | NegSeven
  | NegSix
  | NegFive
  | NegFour
  | NegThree
  | NegTwo
  | NegOne
  | Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Show, Eq, Enum, Ord)

instance Read Digit where
  -- readsPrec :: Int -> ReadS Digit
  readsPrec _ input =
    case span (\c -> isDigit c || c == '-') input of
      (['0'], rest) -> [(Zero, rest)]
      (['1'], rest) -> [(One, rest)]
      (['2'], rest) -> [(Two, rest)]
      (['3'], rest) -> [(Three, rest)]
      (['4'], rest) -> [(Four, rest)]
      (['5'], rest) -> [(Five, rest)]
      (['6'], rest) -> [(Six, rest)]
      (['7'], rest) -> [(Seven, rest)]
      (['8'], rest) -> [(Eight, rest)]
      (['9'], rest) -> [(Nine, rest)]
      ("-0", rest) -> [(Zero, rest)]
      ("-1", rest) -> [(NegOne, rest)]
      ("-2", rest) -> [(NegTwo, rest)]
      ("-3", rest) -> [(NegThree, rest)]
      ("-4", rest) -> [(NegFour, rest)]
      ("-5", rest) -> [(NegFive, rest)]
      ("-6", rest) -> [(NegSix, rest)]
      ("-7", rest) -> [(NegSeven, rest)]
      ("-8", rest) -> [(NegEight, rest)]
      ("-9", rest) -> [(NegNine, rest)]
      _otherwise -> []

data Index = Index {row :: !Int, col :: !Int} deriving (Show, Eq, Ord)

data Grid = Grid {grid :: M.Map Index Digit, width :: Int, height :: Int} deriving (Show)

parseRow :: String -> Maybe [Digit]
parseRow = traverse (readMaybe . (: []))

parseGrid :: String -> Maybe Grid
parseGrid s = do
  rows <- traverse parseRow $ lines s
  let grid =
        M.fromList
          [ (Index rowId colId, val)
            | (rowId, row) <- zip [0 ..] rows,
              (colId, val) <- zip [0 ..] row
          ]
      width = maximum [length row | row <- rows]
      height = length rows
  return $ Grid {grid, width, height}

parseGridFromFile :: String -> IO Grid
parseGridFromFile filePath = do
  contents <- readFile filePath
  case parseGrid contents of
    Nothing -> error "Cannot parse the file"
    Just grid -> return grid

data Direction = U | D | L | R deriving (Show, Enum)

indexesInDirection :: Direction -> Index -> [Index]
indexesInDirection dir = tail . iterate (go dir)
  where
    go U index = index {row = row index - 1}
    go D index = index {row = row index + 1}
    go L index = index {col = col index - 1}
    go R index = index {col = col index + 1}

withinGrid :: Grid -> Index -> Bool
withinGrid Grid {width, height} Index {row, col} =
  row >= 0
    && row < height
    && col >= 0
    && col < width

isOnEdgesGrid :: Grid -> Index -> Bool
isOnEdgesGrid Grid {width, height} Index {row, col} =
  row == 0 || row == (height - 1) || col == 0 || col == (width - 1)

indexesInGrid :: Grid -> [Index]
indexesInGrid Grid {width, height} =
  [ Index rowId colId
    | rowId <- [0 .. height - 1],
      colId <- [0 .. width - 1]
  ]

indexesInDirectionWithinGrid :: Grid -> Direction -> Index -> [Index]
indexesInDirectionWithinGrid grid dir index =
  if withinGrid grid index
    then takeWhile (withinGrid grid) $ indexesInDirection dir index
    else []

higherOrEqualTreeInDirection :: Grid -> Direction -> Index -> Maybe Index
higherOrEqualTreeInDirection g@Grid {grid} dir index = do
  heightOfIndex <- M.lookup index grid
  safeHead . dropWhile (lowerThan heightOfIndex) $ indexesInDirectionWithinGrid g dir index
  where
    lowerThan :: Digit -> Index -> Bool
    lowerThan ht index =
      case M.lookup index grid of
        Just ht' -> ht' < ht
        Nothing -> True

visibleFromDirection :: Grid -> Direction -> Index -> Bool
visibleFromDirection grid dir index =
  case higherOrEqualTreeInDirection grid dir index of
    Just _ -> False
    Nothing -> True

isVisible :: Grid -> Index -> Bool
isVisible grid index =
  isOnEdgesGrid grid index
    || visibleFromDirection grid U index
    || visibleFromDirection grid D index
    || visibleFromDirection grid L index
    || visibleFromDirection grid R index

solvePart1 :: Grid -> Int
solvePart1 grid =
  S.size $
    S.fromList
      [ index
        | index <- indexesInGrid grid,
          isVisible grid index
      ]

scenicScore :: Grid -> Index -> Int
scenicScore grid@Grid {width, height} index@Index {row, col} = scoreUp * scoreDown * scoreLeft * scoreRight
  where
    scoreUp =
      case higherOrEqualTreeInDirection grid U index of
        Just (Index row' _) -> row - row'
        Nothing -> row

    scoreDown =
      case higherOrEqualTreeInDirection grid D index of
        Just (Index row' _) -> row' - row
        Nothing -> height - row - 1

    scoreLeft =
      case higherOrEqualTreeInDirection grid L index of
        Just (Index _ col') -> col - col'
        Nothing -> col

    scoreRight =
      case higherOrEqualTreeInDirection grid R index of
        Just (Index _ col') -> col' - col
        Nothing -> width - col - 1

solvePart2 :: Grid -> Int
solvePart2 grid =
  maximum
    [ scenicScore grid index
      | index <- indexesInGrid grid
    ]

sample :: IO Grid
sample = parseGridFromFile "../sample.txt"

input :: IO Grid
input = parseGridFromFile "../input.txt"

main :: IO ()
main = do
  filePath <- prompt "Enter file path: "
  grid <- parseGridFromFile filePath
  putStrLn $ "Part 1: " ++ show (solvePart1 grid)
  putStrLn $ "Part 2: " ++ show (solvePart2 grid)
