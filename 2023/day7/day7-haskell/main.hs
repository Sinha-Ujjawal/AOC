import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Data.List (group, sort, sortOn)

prompt :: String -> IO String
prompt l = do
    putStr l
    hFlush stdout
    getLine

data Card = Jkr | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | T | J | Q | K | A deriving (Show, Eq, Ord)

type Hand = (Card, Card, Card, Card, Card)

data HandType
    = HighCard
    | OnePair
    | TwoPair
    | ThreeOfKind
    | FullHouse
    | FourOfKind
    | FiveOfKind
    deriving (Show, Eq, Ord, Enum)

type Game = [(Hand, Int)]

handType :: Hand -> HandType
handType (_1, _2, _3, _4, _5) =
    case sortOn length . group $ sort [_1, _2, _3, _4, _5] of
        [[_, _, _, _, _]] -> FiveOfKind

        [[x], [y, _, _, _]] ->
            if x == Jkr || y == Jkr
                then FiveOfKind
                else FourOfKind

        [[x, _], [y, _, _]] ->
            if x == Jkr || y == Jkr
                then FiveOfKind
                else FullHouse

        [[x], [y], [z, _, _]] ->
            if x == Jkr || y == Jkr || z == Jkr
                then FourOfKind
                else ThreeOfKind

        [[x], [y, _], [z, _]] ->
            if x == Jkr
                then FullHouse
                else if y == Jkr || z == Jkr
                    then FourOfKind
                    else TwoPair

        [[x], [y], [z], [w, _]] ->
            if x == Jkr || y == Jkr || z == Jkr || w == Jkr
                then ThreeOfKind
                else OnePair

        [[x], [y], [z], [w], [a]] ->
            if x == Jkr || y == Jkr || z == Jkr || w == Jkr || a == Jkr
                then OnePair
                else HighCard

parseCard :: Char -> Maybe Card
parseCard 'A' = Just A
parseCard 'K' = Just K
parseCard 'Q' = Just Q
parseCard 'J' = Just J
parseCard 'T' = Just T
parseCard '9' = Just C9
parseCard '8' = Just C8
parseCard '7' = Just C7
parseCard '6' = Just C6
parseCard '5' = Just C5
parseCard '4' = Just C4
parseCard '3' = Just C3
parseCard '2' = Just C2
parseCard _   = Nothing

parseHand :: String -> Maybe Hand
parseHand [_1s, _2s, _3s, _4s, _5s] = do
    _1 <- parseCard _1s
    _2 <- parseCard _2s
    _3 <- parseCard _3s
    _4 <- parseCard _4s
    _5 <- parseCard _5s
    return (_1, _2, _3, _4, _5)
parseHand _ = Nothing

parseHandAndScoreFromString :: String -> Maybe (Hand, Int)
parseHandAndScoreFromString = go . words
  where
    go [handStr, scoreStr] = do
        hand <- parseHand handStr
        score <- readMaybe scoreStr :: Maybe Int
        return (hand, score)
    go _ = Nothing

parseGameFromString :: String -> Maybe Game
parseGameFromString = traverse parseHandAndScoreFromString . lines

parseGameFromFile :: String -> IO (Maybe Game)
parseGameFromFile = fmap parseGameFromString . readFile

solvePart1ForGame :: Game -> Int
solvePart1ForGame =
    sum
    . fmap (\(r, (_, s)) -> r*s)
    . zip [1..]
    . sortOn fst
    . fmap (\(h, s) -> ((handType h, h), s))

solvePart1ForFile :: String -> IO Int
solvePart1ForFile filePath = do
    gameMaybe <- parseGameFromFile filePath
    case gameMaybe of
        Nothing -> do
            putStrLn $ "Cannot parse the file: " ++ filePath
            return (-1)
        Just game -> return $ solvePart1ForGame game

solvePart2ForGame :: Game -> Int
solvePart2ForGame = solvePart1ForGame . fmap joker
  where
    joker ((_1, _2, _3, _4, _5), s) =
        let _1' = if _1 == J then Jkr else _1
            _2' = if _2 == J then Jkr else _2
            _3' = if _3 == J then Jkr else _3
            _4' = if _4 == J then Jkr else _4
            _5' = if _5 == J then Jkr else _5
        in
            ((_1', _2', _3', _4', _5'), s)

solvePart2ForFile :: String -> IO Int
solvePart2ForFile filePath = do
    gameMaybe <- parseGameFromFile filePath
    case gameMaybe of
        Nothing -> do
            putStrLn $ "Cannot parse the file: " ++ filePath
            return (-1)
        Just game -> return $ solvePart2ForGame game

main :: IO ()
main = do
    filePath <- prompt "Enter file path: "
    gameMaybe <- parseGameFromFile filePath
    case gameMaybe of
        Nothing -> putStrLn $ "Cannot parse the file: " ++ filePath
        Just game -> do
            let part1Ans = solvePart1ForGame game
                part2Ans = solvePart2ForGame game
            putStrLn $ "Part 1: " ++ show part1Ans
            putStrLn $ "Part 2: " ++ show part2Ans
