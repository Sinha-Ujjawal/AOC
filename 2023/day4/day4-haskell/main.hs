{-# LANGUAGE OverloadedStrings #-}

import System.IO (hFlush, stdout)
import Control.Monad ((<=<))
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Read (readMaybe)
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Bifunctor as Bif

prompt :: String -> IO String
prompt l = do
    putStr l
    hFlush stdout
    getLine

data Card = Card { cardID :: Int, winningNumbers :: S.Set Int, cardNumbers :: S.Set Int } deriving (Show, Eq)

matchingNumbers :: Card -> S.Set Int
matchingNumbers Card{winningNumbers, cardNumbers} = S.intersection winningNumbers cardNumbers

parseCardFromString :: T.Text -> Maybe Card
parseCardFromString = go <=< fmap (T.splitOn ": ") . T.stripPrefix "Card "
  where
    go :: [T.Text] -> Maybe Card
    go [cardIDStr, rest] = do
        cardID <- readMaybe $ T.unpack cardIDStr :: Maybe Int
        (winningNumbers, cardNumbers) <- go' rest
        return $ Card { cardID = cardID, winningNumbers = winningNumbers, cardNumbers = cardNumbers }
    go _ = Nothing

    go' :: T.Text -> Maybe (S.Set Int, S.Set Int)
    go' =  go'' . T.splitOn " | "

    go'' :: [T.Text] -> Maybe (S.Set Int, S.Set Int)
    go'' [winningNumbersStr, cardNumbersStr] = do
        winningNumbers <- parseNumbers winningNumbersStr
        cardNumbers <- parseNumbers cardNumbersStr
        return (winningNumbers, cardNumbers)
    go'' _ = Nothing

    parseNumbers :: T.Text -> Maybe (S.Set Int)
    parseNumbers =
        fmap S.fromList
        . traverse (readMaybe . T.unpack)
        . filter (/= "")
        . T.splitOn " "

parseCardsFromString :: T.Text -> Maybe [Card]
parseCardsFromString = traverse parseCardFromString . T.lines

parseCardsFromFile :: String -> IO (Maybe [Card])
parseCardsFromFile = fmap parseCardsFromString . T.readFile

solvePart1ForInput :: [Card] -> Int
solvePart1ForInput = sum . fmap cardPoint
  where
    cardPoint :: Card -> Int
    cardPoint card =
        let sz = S.size $ matchingNumbers card
        in if sz == 0 then 0 else 2 ^ (sz - 1)

solvePart1ForFile :: String -> IO Int
solvePart1ForFile filePath = do
    fileContent <- T.readFile filePath
    case parseCardsFromString fileContent of
        Nothing -> do
            putStrLn ("Could not parse the file: " ++ filePath)
            return (-1)
        Just cards -> return $ solvePart1ForInput cards

solvePart2ForInput :: [Card] -> Int
solvePart2ForInput = go 0 . fmap (, 1)
  where
    go acc [] = acc
    go acc ((card, cnt):cards) =
        let sz            = S.size $ matchingNumbers card
            (left, right) = L.splitAt sz cards
            rest          = fmap (Bif.second (cnt +)) left ++ right
        in go (acc+cnt) rest

solvePart2ForFile :: String -> IO Int
solvePart2ForFile filePath = do
    fileContent <- T.readFile filePath
    case parseCardsFromString fileContent of
        Nothing -> do
            putStrLn ("Could not parse the file: " ++ filePath)
            return (-1)
        Just cards -> return $ solvePart2ForInput cards

main :: IO ()
main = do
    filePath <- prompt "Enter file path: "
    fileContent <- T.readFile filePath
    case parseCardsFromString fileContent of
        Nothing -> putStrLn $ "Could not parse the file: " ++ filePath
        Just cards -> do
            let part1Ans = solvePart1ForInput cards
                part2Ans = solvePart2ForInput cards
            putStrLn $ "Part 1: " ++ show part1Ans
            putStrLn $ "Part 2: " ++ show part2Ans
