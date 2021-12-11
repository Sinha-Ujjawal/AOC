import Data.List (sort)
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

withIndex :: [a] -> [(Int, a)]
withIndex = zip [0 ..]

median :: (Ord a, Num a, Fractional a) => [a] -> Maybe a
median [] = Nothing
median xs =
  Just $
    if even l
      then (xs !! l' + xs !! (l' + 1)) / 2
      else xs' !! l'
  where
    xs' = sort xs
    l = length xs'
    l' = div l 2

data Bracket = Circular | Square | Curly | Angle deriving (Show, Eq)

newtype Open b = Open b deriving (Show, Eq)

newtype Close b = Close b deriving (Show, Eq)

type Symbol = Either (Open Bracket) (Close Bracket)

data Error
  = Corrupted (Int, Open Bracket) (Int, Close Bracket)
  | OpenUnbalanced [Open Bracket]
  | CloseUnbalanced
  deriving (Show, Eq)

isOpen :: Symbol -> Bool
isOpen (Left _) = True
isOpen (Right _) = False

isClosed :: Symbol -> Bool
isClosed = not . isOpen

match :: Symbol -> Symbol -> Bool
match (Left (Open b)) (Right (Close b')) = b == b'
match _ _ = False

bracket :: Symbol -> Bracket
bracket (Left (Open b)) = b
bracket (Right (Close b)) = b

scoreError :: Error -> Int
scoreError (OpenUnbalanced {}) = 0
scoreError (CloseUnbalanced {}) = 0
scoreError (Corrupted _ (_, rs)) =
  case bracket (Right rs) of
    Circular -> 3
    Square -> 57
    Curly -> 1197
    Angle -> 25137

isFixableError :: Error -> Bool
isFixableError (OpenUnbalanced {}) = True
isFixableError _ = False

scoreFix :: Error -> Int
scoreFix (Corrupted {}) = 0
scoreFix (CloseUnbalanced {}) = 0
scoreFix (OpenUnbalanced openBrackets) = foldl score 0 openBrackets
  where
    score s (Open b) = s * 5 + scoreBracket b
    scoreBracket Circular = 1
    scoreBracket Square = 2
    scoreBracket Curly = 3
    scoreBracket Angle = 4

parseLineMaybe :: String -> Maybe [Symbol]
parseLineMaybe = traverse go
  where
    go '(' = Just . Left $ Open Circular
    go ')' = Just . Right $ Close Circular
    go '[' = Just . Left $ Open Square
    go ']' = Just . Right $ Close Square
    go '{' = Just . Left $ Open Curly
    go '}' = Just . Right $ Close Curly
    go '<' = Just . Left $ Open Angle
    go '>' = Just . Right $ Close Angle
    go _ = Nothing

parseLinesMaybe :: [String] -> Maybe [[Symbol]]
parseLinesMaybe = traverse parseLineMaybe

compileLine :: [Symbol] -> Maybe Error
compileLine = go [] . withIndex
  where
    go [] [] = Nothing
    go stack [] = Just . OpenUnbalanced $ map snd stack
    go stack (hd : rest) =
      case hd of
        (i, Left b@(Open _)) -> go ((i, b) : stack) rest
        y@(i, Right b@(Close _)) ->
          case validate stack (i, b) of
            err@(Just _) -> err
            Nothing -> go (tail stack) rest

    validate stack y =
      case stack of
        [] -> Just CloseUnbalanced
        (x : _) -> validateSymbolPair x y

    validateSymbolPair x@(i, symbol@(Open _)) y@(j, symbol'@(Close _)) =
      if match (Left symbol) (Right symbol')
        then Nothing
        else Just $ Corrupted x y

compile :: String -> Maybe [Error]
compile program = do
  ls <- parseLinesMaybe $ lines program
  traverse compileLine ls

solvePart1 :: String -> Maybe Int
solvePart1 program = do
  errs <- compile program
  Just $ sum $ map scoreError errs

solvePart2 :: String -> Maybe Int
solvePart2 program = do
  errs <- filter isFixableError <$> compile program
  fmap floor $ median $ map (fromIntegral . scoreFix) errs

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  content <- readFile filename

  let part1Ans = solvePart1 content
      part2Ans = solvePart2 content

  putStrLn $ "Part1: " ++ show part1Ans
  putStrLn $ "Part2: " ++ show part2Ans