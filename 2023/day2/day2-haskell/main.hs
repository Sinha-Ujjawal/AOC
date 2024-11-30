{-# LANGUAGE OverloadedStrings #-}

import System.IO (hFlush, stdout)
import Control.Monad ((<=<))
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T

prompt :: String -> IO String
prompt line = do
    putStr line
    hFlush stdout
    getLine

data Cube = Red | Blue | Green deriving (Show, Eq)
data Game = Game { gameId :: Int, gameCubes :: [[(Int, Cube)]] } deriving (Show, Eq)

-- 3 blue
-- 4 red
-- 2 green
parseCubeFromString :: T.Text -> Maybe (Int, Cube)
parseCubeFromString = go . T.splitOn " "
  where
    go [numCubesStr, cubeStr] = do
        numCubes <- readMaybe $ T.unpack numCubesStr :: Maybe Int
        cube <-
            case cubeStr of
                "red"   -> Just Red
                "blue"  -> Just Blue
                "green" -> Just Green
                _       -> Nothing
        return (numCubes, cube)
    go _ = Nothing

-- 3 blue, 4 red
parseCubesFromString :: T.Text -> Maybe [(Int, Cube)]
parseCubesFromString = traverse parseCubeFromString . T.splitOn ", "

-- Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
parseGameFromString :: T.Text -> Maybe Game
parseGameFromString = go . T.splitOn ": " <=< T.stripPrefix "Game "
  where
    go [gameIdStr, cubesStr] = do
        gameId <- readMaybe $ T.unpack gameIdStr :: Maybe Int
        gameCubes <- traverse parseCubesFromString $ T.splitOn "; " cubesStr
        return $ Game { gameId = gameId, gameCubes = gameCubes }
    go _ = Nothing

parseGamesFromString :: T.Text -> Maybe [Game]
parseGamesFromString = traverse parseGameFromString . T.lines

parseGamesFromFile :: String -> IO (Maybe [Game])
parseGamesFromFile = fmap parseGamesFromString . T.readFile

solvePart1ForInput :: [Game] -> Int
solvePart1ForInput = sum . fmap gameId . filter isValidGame
    where
        isValidCubes = go 0 0 0
            where
                go r g b []              = r <= 12 && g <= 13 && b <= 14
                go r g b ((n, Red):cs)   = go (r+n) g    b    cs
                go r g b ((n, Green):cs) = go  r   (g+n) b    cs
                go r g b ((n, Blue):cs)  = go  r    g   (b+n) cs

        isValidGame = and . fmap isValidCubes . gameCubes

solvePart1ForFile :: String -> IO Int
solvePart1ForFile filePath = do
    gamesMaybe <- parseGamesFromFile filePath
    case gamesMaybe of
        Just games ->
            return $ solvePart1ForInput games
        Nothing -> do
            putStrLn $ "Could not parse the file: " ++ filePath
            return (-1)

solvePart2ForInput :: [Game] -> Int
solvePart2ForInput = sum . fmap power
  where
    power = go 0 0 0 . (=<<) id . gameCubes

    go r g b []              = r * g * b
    go r g b ((n, Red):xs)   = go (max r n) g        b        xs
    go r g b ((n, Green):xs) = go  r       (max g n) b        xs
    go r g b ((n, Blue):xs)  = go  r        g       (max b n) xs

solvePart2ForFile :: String -> IO Int
solvePart2ForFile filePath = do
    gamesMaybe <- parseGamesFromFile filePath
    case gamesMaybe of
        Just games ->
            return $ solvePart2ForInput games
        Nothing -> do
            putStrLn $ "Could not parse the file: " ++ filePath
            return (-1)

main :: IO ()
main = do
    filePath <- prompt "Enter File Path: "
    gamesMaybe <- parseGamesFromFile filePath
    case gamesMaybe of
        Just games -> do
            let part1Ans = solvePart1ForInput games
                part2Ans = solvePart2ForInput games
            putStrLn $ "Part 1: " ++ show part1Ans
            putStrLn $ "Part 2: " ++ show part2Ans
        Nothing ->
            putStrLn $ "Could not parse the file: " ++ filePath
