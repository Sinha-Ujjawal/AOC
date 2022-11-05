{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad (forM_)
import qualified Data.Array.Base as AIO
import qualified Data.Array.IO as AIO
import Data.Bits (xor)
import qualified Data.Text as T
import qualified System.Environment as SE
import System.IO (hFlush, stdout)
import qualified Text.Read as T

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

type Coordinate = (Int, Int)

type Box = (Coordinate, Coordinate)

type Brightness = Int

type LightGrid = AIO.IOUArray Int Int

initLights :: IO LightGrid
initLights = AIO.newArray (0, 1000 * 1000) 0

forLoop :: (Monad m) => a -> (a -> Bool) -> (a -> a) -> (a -> m ()) -> m ()
forLoop start cond inc f = go start
  where
    go !x
      | cond x = f x >> go (inc x)
      | otherwise = return ()
{-# INLINE forLoop #-}

type BrightnessUpdateFn = (Brightness -> Brightness)

updateGrid :: BrightnessUpdateFn -> Box -> LightGrid -> IO LightGrid
updateGrid updateFn ((xl, yl), (xu, yu)) grid = do
  forLoop xl (<= xu) (+ 1) $ \row -> do
    forLoop yl (<= yu) (+ 1) $ \col -> do
      let idx = row * 1000 + col
      oldValue <- AIO.readArray grid idx
      AIO.writeArray grid idx (max 0 (updateFn oldValue))
  return grid

data Instruction = TurnOn !Box | TurnOff !Box | Toggle !Box deriving (Show, Eq)

parseCoordinate :: T.Text -> Maybe Coordinate
parseCoordinate (T.splitOn "," -> [x, y]) = do
  x' <- T.readMaybe $ T.unpack x
  y' <- T.readMaybe $ T.unpack y
  return (x', y')
parseCoordinate _ = Nothing

parseBox :: T.Text -> Maybe Box
parseBox (T.splitOn " through " -> [startCoordText, endCoordText]) = do
  startCoord <- parseCoordinate startCoordText
  endCoord <- parseCoordinate endCoordText
  return (startCoord, endCoord)
parseBox _ = Nothing

parseLine :: T.Text -> Maybe Instruction
parseLine (T.stripPrefix "turn on " -> Just rest) = fmap TurnOn (parseBox rest)
parseLine (T.stripPrefix "turn off " -> Just rest) = fmap TurnOff (parseBox rest)
parseLine (T.stripPrefix "toggle " -> Just rest) = fmap Toggle (parseBox rest)
parseLine _ = Nothing

parseLines :: [T.Text] -> Maybe [Instruction]
parseLines = traverse parseLine

solve :: (BrightnessUpdateFn, BrightnessUpdateFn, BrightnessUpdateFn) -> [Instruction] -> IO LightGrid
solve (turnOnUpdateFn, turnOffUpdateFn, toggleUpdateFn) instructions = do
  grid <- initLights
  forM_ instructions $ \instruction -> do
    case instruction of
      TurnOn box -> updateGrid turnOnUpdateFn box grid
      TurnOff box -> updateGrid turnOffUpdateFn box grid
      Toggle box -> updateGrid toggleUpdateFn box grid
  return grid

solvePart1 :: [Instruction] -> IO Int
solvePart1 instructions = do
  grid <- solve (turnOnUpdateFn, turnOffUpdateFn, toggleUpdateFn) instructions
  elems <- AIO.getElems grid
  return . length $ filter (> 0) elems
  where
    turnOnUpdateFn = const 1
    turnOffUpdateFn = const 0
    toggleUpdateFn b = b `xor` 1

solvePart2 :: [Instruction] -> IO Int
solvePart2 instructions = do
  grid <- solve (turnOnUpdateFn, turnOffUpdateFn, toggleUpdateFn) instructions
  elems <- AIO.getElems grid
  return . sum $ filter (> 0) elems
  where
    turnOnUpdateFn = (+) 1
    turnOffUpdateFn b = if b > 0 then b - 1 else 0
    toggleUpdateFn = (+) 2

main :: IO ()
main = do
  args <- SE.getArgs
  filename <- case args of
    [filename] -> return filename
    _ -> prompt "Enter file name: "
  fileLines <- T.lines . T.pack <$> readFile filename

  let instructions = case parseLines fileLines of
        Just instructions -> instructions
        Nothing -> error "Unable to parse the file into instructions!"
  part1Ans <- solvePart1 instructions
  part2Ans <- solvePart2 instructions

  putStrLn $ "Part 1: " ++ show part1Ans
  putStrLn $ "Part 2: " ++ show part2Ans
