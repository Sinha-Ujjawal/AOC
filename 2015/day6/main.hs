{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
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

type LightGrid = M.Map Coordinate Brightness

initLights :: LightGrid
initLights = M.empty

boxCoordinates :: Box -> [Coordinate]
boxCoordinates ((xl, yl), (xu, yu)) =
  [(x, y) | x <- [xl .. xu], y <- [yl .. yu]]

type BrightnessUpdateFn = (Coordinate -> Brightness -> Brightness)

updateBrightnessBy :: BrightnessUpdateFn -> LightGrid -> Coordinate -> LightGrid
updateBrightnessBy updateFn grid coord = M.alter go coord grid
  where
    go Nothing = go (Just 0)
    go (Just b) =
      let b' = updateFn coord b
       in if b' <= 0 then Nothing else Just b'

updateGrid :: BrightnessUpdateFn -> Box -> LightGrid -> LightGrid
updateGrid updateFn box grid = foldr (flip $ updateBrightnessBy updateFn) grid $ boxCoordinates box

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

solvePart1 :: [Instruction] -> Int
solvePart1 = M.size . foldr applyInstruction initLights . reverse
  where
    applyInstruction :: Instruction -> LightGrid -> LightGrid
    applyInstruction (TurnOn box) = turnOn box
    applyInstruction (TurnOff box) = turnOff box
    applyInstruction (Toggle box) = toggle box

    turnOn = updateGrid (const (const 1))
    turnOff = updateGrid (const (const 0))
    toggle box grid = updateGrid go box grid
      where
        go coord _
          | M.member coord grid = 0
          | otherwise = 1

solvePart2 :: [Instruction] -> Int
solvePart2 = sum . M.elems . foldr applyInstruction initLights . reverse
  where
    applyInstruction :: Instruction -> LightGrid -> LightGrid
    applyInstruction (TurnOn box) = turnOn box
    applyInstruction (TurnOff box) = turnOff box
    applyInstruction (Toggle box) = toggle box

    turnOn = updateGrid (const (+ 1))
    turnOff = updateGrid (const (\x -> x - 1))
    toggle = updateGrid (const (+ 2))

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  fileLines <- T.lines . T.pack <$> readFile filename

  let instructions = fromMaybe [] $ parseLines fileLines
      part1Ans = solvePart1 instructions
      part2Ans = solvePart2 instructions

  putStrLn $ "Part 1: " ++ show part1Ans
  putStrLn $ "Part 2: " ++ show part2Ans
