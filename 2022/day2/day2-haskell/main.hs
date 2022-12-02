{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine

data Hand = Rock | Paper | Scissor deriving (Eq, Show)

data Outcome = Won | Lost | Tie deriving (Eq, Show)

data Play = Play {opponentPlay :: !Hand, youPlay :: !Hand} deriving (Eq, Show)

toOutcome :: Play -> Outcome
toOutcome Play {opponentPlay, youPlay} =
  case (youPlay, opponentPlay) of
    (Rock, Scissor) -> Won
    (Scissor, Paper) -> Won
    (Paper, Rock) -> Won
    (youPlay, opponentPlay) -> if youPlay == opponentPlay then Tie else Lost

decideHand :: Hand -> Outcome -> Hand
decideHand Rock Lost = Scissor
decideHand Paper Lost = Rock
decideHand Scissor Lost = Paper
decideHand Rock Won = Paper
decideHand Paper Won = Scissor
decideHand Scissor Won = Rock
decideHand x Tie = x

scorePlays :: [Play] -> Int
scorePlays = sum . fmap score
  where
    score :: Play -> Int
    score play = scoreHand (youPlay play) + scoreOutcome (toOutcome play)

    scoreHand :: Hand -> Int
    scoreHand Rock = 1
    scoreHand Paper = 2
    scoreHand Scissor = 3

    scoreOutcome :: Outcome -> Int
    scoreOutcome Won = 6
    scoreOutcome Tie = 3
    scoreOutcome Lost = 0

solvePart1 :: T.Text -> IO Int
solvePart1 s = do
  case parsePlays s of
    Nothing -> error "Could not parse the string"
    Just plays -> return $ scorePlays plays
  where
    parsePlays :: T.Text -> Maybe [Play]
    parsePlays = traverse parsePlay . T.splitOn "\n" . T.strip
      where
        parsePlay = parsePlay' . T.splitOn " "

        parsePlay' [handOpponentStr, handYourStr] = do
          handOpponent <- parseHand handOpponentStr
          handYour <- parseHand handYourStr
          return Play {opponentPlay = handOpponent, youPlay = handYour}
        parsePlay' _ = Nothing

        parseHand "A" = Just Rock
        parseHand "X" = Just Rock
        parseHand "B" = Just Paper
        parseHand "Y" = Just Paper
        parseHand "C" = Just Scissor
        parseHand "Z" = Just Scissor
        parseHand _ = Nothing

solvePart2 :: T.Text -> IO Int
solvePart2 s = do
  case parsePlays s of
    Nothing -> error "Could not parse the string"
    Just plays -> return $ scorePlays plays
  where
    parsePlays :: T.Text -> Maybe [Play]
    parsePlays = traverse parsePlay . T.splitOn "\n" . T.strip
      where
        parsePlay = parsePlay' . T.splitOn " "

        parsePlay' [handOpponentStr, outcomeStr] = do
          handOpponent <- parseHand handOpponentStr
          outcome <- parseOutcome outcomeStr
          let handYour = decideHand handOpponent outcome
          return Play {opponentPlay = handOpponent, youPlay = handYour}
        parsePlay' _ = Nothing

        parseHand "A" = Just Rock
        parseHand "B" = Just Paper
        parseHand "C" = Just Scissor
        parseHand _ = Nothing

        parseOutcome "X" = Just Lost
        parseOutcome "Y" = Just Tie
        parseOutcome "Z" = Just Won
        parseOutcome _ = Nothing

main :: IO ()
main = do
  filename <- prompt "Enter file path: "
  contents <- T.pack <$> readFile filename
  part1Ans <- solvePart1 contents
  putStrLn $ "Part 1: " ++ show part1Ans
  part2Ans <- solvePart2 contents
  putStrLn $ "Part 2: " ++ show part2Ans
