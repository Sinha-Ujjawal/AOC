{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

import System.IO (hFlush, stdout)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (maybe)
import Data.List (nub)

prompt :: String -> IO String
prompt l = do
    putStr l
    hFlush stdout
    getLine

data Direction = L | R deriving (Show, Eq)

type Node = T.Text
type Map = M.Map Node (Node, Node)
type Directions = [Direction]

followDirections :: Map -> [Node] -> Directions -> [[Maybe Node]]
followDirections m nodes = scanl followDirection (fmap Just nodes)
  where
    followDirection :: [Maybe Node] -> Direction -> [Maybe Node]
    followDirection nodes L = nub $ fmap (fmap fst . (=<<) (`M.lookup` m)) nodes
    followDirection nodes R = nub $ fmap (fmap snd . (=<<) (`M.lookup` m)) nodes

parseMapFromString :: T.Text -> Maybe (Directions, Map)
parseMapFromString = parse . T.lines
  where
    parse :: [T.Text] -> Maybe (Directions, Map)
    parse (directionsStr : "" : nodesStr) = do
        directions <- parseDirections directionsStr
        nodes      <- parseNodes nodesStr
        return (directions, M.fromList nodes)

    parseDirections :: T.Text -> Maybe [Direction]
    parseDirections = traverse parseDirection . T.unpack

    parseDirection :: Char -> Maybe Direction
    parseDirection 'L' = Just L
    parseDirection 'R' = Just R
    parseDirection _   = Nothing

    parseNodes :: [T.Text] -> Maybe [(Node, (Node, Node))]
    parseNodes = traverse parseNode

    parseNode :: T.Text -> Maybe (Node, (Node, Node))
    parseNode s =
        case T.splitOn " = " s of
            [start, T.splitOn ", " -> [T.stripPrefix "(" -> Just left, T.stripSuffix ")" -> Just right]] ->
                Just (start, (left, right))
            _ -> Nothing

parseMapFromFile :: String -> IO (Maybe (Directions, Map))
parseMapFromFile = fmap parseMapFromString . T.readFile

solvePart1ForInput :: (Directions, Map) -> Int
solvePart1ForInput (directions, m) =
    fst
    . head
    . dropWhile ((/= [Just "ZZZ"]) . snd)
    . takeWhile ((/= [Nothing]) . snd)
    . zip [0..]
    . followDirections m ["AAA"]
    $ cycle directions

solvePart1ForFile :: String -> IO Int
solvePart1ForFile filePath = do
    inputMaybe <- parseMapFromFile filePath
    case inputMaybe of
        Nothing -> do
            putStrLn $ "Could not able to parse the file: " ++ filePath
            return (-1)
        Just input -> return $ solvePart1ForInput input

solvePart2ForInput :: (Directions, Map) -> Int
solvePart2ForInput (directions, m) =
    foldl lcm 1 $ stepsToZ <$> startingNodes
    where
        startingNodes = filter (T.isSuffixOf "A") $ M.keys m
        allEndsWithZ = all (maybe False (T.isSuffixOf "Z"))

        stepsToZ node =
            fst
            . head
            . dropWhile (not . allEndsWithZ . snd)
            . zip [0..]
            . followDirections m [node]
            $ cycle directions

solvePart2ForFile :: String -> IO Int
solvePart2ForFile filePath = do
    inputMaybe <- parseMapFromFile filePath
    case inputMaybe of
        Nothing -> do
            putStrLn $ "Could not able to parse the file: " ++ filePath
            return (-1)
        Just input -> return $ solvePart2ForInput input

main :: IO ()
main = do
    filePath <- prompt "Enter file path: "
    inputMaybe <- parseMapFromFile filePath
    case inputMaybe of
        Nothing -> putStrLn $ "Could not able to parse the file: " ++ filePath
        Just input -> do
            let part1Ans = solvePart1ForInput input
                part2Ans = solvePart2ForInput input
            putStrLn $ "Part 1: " ++ show part1Ans
            putStrLn $ "Part 2: " ++ show part2Ans
