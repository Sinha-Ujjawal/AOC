{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (Alternative ((<|>)))
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine

nunique :: Ord a => [a] -> Int
nunique = S.size . S.fromList

readMaybeText :: Read a => T.Text -> Maybe a
readMaybeText = readMaybe . T.unpack

data Direction = U | D | L | R deriving (Show, Eq)

type Instruction = (Direction, Int)

type Position = (Int, Int)

positionDelta :: Position -> Position -> (Int, Int)
positionDelta (y2, x2) (y1, x1) = (y2 - y1, x2 - x1)

--   o---------col------->  x-axis
--   |
--   |
--  row
--   |
--   |
--   v
--  y-axis

addPosition :: Position -> (Int, Int) -> Position
addPosition (y, x) (dy, dx) = (y + dy, x + dx)

movePosition :: Position -> Direction -> Position
movePosition pos U = addPosition pos (-1, 0)
movePosition pos D = addPosition pos (1, 0)
movePosition pos L = addPosition pos (0, -1)
movePosition pos R = addPosition pos (0, 1)

unrollInstruction :: Instruction -> [Direction]
unrollInstruction (dir, by) = replicate by dir

type ForceMap = ((Int, Int) -> Maybe (Int, Int))

-- moveRope2 :: ForceMap -> (Position, Position) -> Direction -> (Position, Position)
-- moveRope2 forceMap (knotPos, prevKnotPos) dir = (newKnotPos, newPrevKnotPos)
--   where
--     newKnotPos = movePosition knotPos dir
--     delta = positionDelta newKnotPos prevKnotPos
--     newPrevKnotPos = addPosition prevKnotPos (fromMaybe (0, 0) $ forceMap delta)

follow :: ForceMap -> [Position] -> [Position]
follow _ [] = []
follow _ [knot] = [knot]
follow forceMap knots@(movedKnot : knot : rest) =
  case forceMap delta of
    Just force -> movedKnot : follow forceMap (addPosition knot force : rest)
    Nothing -> knots
  where
    delta = positionDelta movedKnot knot

solve :: ForceMap -> Int -> [Instruction] -> Int
solve forceMap nKnots =
  if nKnots >= 2
    then
      nunique
        . fmap last
        . scanl updateKnots (replicate nKnots (0, 0))
        . (=<<) unrollInstruction
    else const 0
  where
    updateKnots [] _ = error "Unreachable!"
    updateKnots (headKnot : rest) dir = follow forceMap (movePosition headKnot dir : rest)

forceMap1 :: ForceMap
forceMap1 (-2, 0) = Just (-1, 0)
forceMap1 (2, 0) = Just (1, 0)
forceMap1 (0, -2) = Just (0, -1)
forceMap1 (0, 2) = Just (0, 1)
forceMap1 (-2, -1) = Just (-1, -1)
forceMap1 (-2, 1) = Just (-1, 1)
forceMap1 (2, -1) = Just (1, -1)
forceMap1 (2, 1) = Just (1, 1)
forceMap1 (-1, -2) = Just (-1, -1)
forceMap1 (1, -2) = Just (1, -1)
forceMap1 (-1, 2) = Just (-1, 1)
forceMap1 (1, 2) = Just (1, 1)
forceMap1 _ = Nothing

solvePart1 :: [Instruction] -> Int
solvePart1 = solve forceMap1 2

forceMap2 :: ForceMap
forceMap2 (-2, -2) = Just (-1, -1)
forceMap2 (-2, 2) = Just (-1, 1)
forceMap2 (2, -2) = Just (1, -1)
forceMap2 (2, 2) = Just (1, 1)
forceMap2 any = forceMap1 any

solvePart2 :: [Instruction] -> Int
solvePart2 = solve forceMap2 10

parseInstruction :: T.Text -> Maybe Instruction
parseInstruction line =
  Nothing
    <|> parseInstruction' 'U' U line
    <|> parseInstruction' 'D' D line
    <|> parseInstruction' 'L' L line
    <|> parseInstruction' 'R' R line
  where
    parseInstruction' :: Char -> Direction -> T.Text -> Maybe Instruction
    parseInstruction' char dir line = do
      num <- readMaybeText =<< T.stripPrefix (T.pack [char, ' ']) line
      if num <= 0
        then Nothing
        else return (dir, num)

parseInstructions :: T.Text -> Maybe [Instruction]
parseInstructions = traverse parseInstruction . T.lines

parseFromFilePath :: FilePath -> IO [Instruction]
parseFromFilePath filePath = do
  contents <- T.pack <$> readFile filePath
  case parseInstructions contents of
    Nothing -> error "Unable to parse file"
    Just instructions -> return instructions

sample :: IO [Instruction]
sample = parseFromFilePath "../sample.txt"

sample2 :: IO [Instruction]
sample2 = parseFromFilePath "../sample2.txt"

input :: IO [Instruction]
input = parseFromFilePath "../input.txt"

main :: IO ()
main = do
  filePath <- prompt "Enter file path: "
  instructions <- parseFromFilePath filePath
  putStrLn $ "Part 1: " ++ show (solvePart1 instructions)
  putStrLn $ "Part 2: " ++ show (solvePart2 instructions)
