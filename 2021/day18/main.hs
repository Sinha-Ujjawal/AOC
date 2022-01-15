import Data.Char (digitToInt, isDigit)
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

parseInt :: String -> (Maybe Int, String)
parseInt = go Nothing
  where
    go number [] = (number, [])
    go number xs'@(x : xs)
      | not $ isDigit x = (number, xs')
      | otherwise =
        case number of
          Just n -> go (Just (n * 10 + digitToInt x)) xs
          Nothing -> go (Just (digitToInt x)) xs

subsets :: (Eq t, Num t) => t -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs

data SnailFish
  = Pair SnailFish SnailFish
  | Leaf Int
  deriving (Eq)

instance Show SnailFish where
  show (Leaf x) = show x
  show (Pair l r) = "[" ++ show l ++ "," ++ show r ++ "]"

inorderSnailfish :: SnailFish -> [Int]
inorderSnailfish (Leaf x) = [x]
inorderSnailfish (Pair l r) = inorderSnailfish l ++ inorderSnailfish r

magnitudeSnailfish :: SnailFish -> Int
magnitudeSnailfish (Leaf x) = x
magnitudeSnailfish (Pair l r) =
  let leftMag = magnitudeSnailfish l
      rightMag = magnitudeSnailfish r
   in (3 * leftMag + 2 * rightMag)

rightmostChild :: SnailFish -> SnailFish
rightmostChild lf@(Leaf _) = lf
rightmostChild (Pair _ r) = rightmostChild r

leftmostChild :: SnailFish -> SnailFish
leftmostChild lf@(Leaf _) = lf
leftmostChild (Pair l _) = leftmostChild l

addToRightmostChild :: Int -> SnailFish -> SnailFish
addToRightmostChild x = go
  where
    go (Leaf y) = Leaf (x + y)
    go (Pair l r) = Pair l (go r)

addToLeftmostChild :: Int -> SnailFish -> SnailFish
addToLeftmostChild x = go
  where
    go (Leaf y) = Leaf (x + y)
    go (Pair l r) = Pair (go l) r

explodeSnailfish :: SnailFish -> (SnailFish, Bool)
explodeSnailfish = (\(s, b, _, _) -> (s, b)) . go 0
  where
    go _ lf@(Leaf _) = (lf, False, Nothing, Nothing)
    go 4 (Pair (Leaf x) (Leaf y)) = (Leaf 0, True, Just x, Just y)
    go d pr@(Pair l r) =
      let leftPart = go (d + 1) l
          rightPart = go (d + 1) r
       in case leftPart of
            (l', True, x, Just y) -> (Pair l' (addToLeftmostChild y r), True, x, Nothing)
            (l', True, x, Nothing) -> (Pair l' r, True, x, Nothing)
            _ ->
              case rightPart of
                (r', True, Just x, y) -> (Pair (addToRightmostChild x l) r', True, Nothing, y)
                (r', True, Nothing, y) -> (Pair l r', True, Nothing, y)
                _ -> (Pair l r, False, Nothing, Nothing)

splitSnailfish :: SnailFish -> (SnailFish, Bool)
splitSnailfish = go
  where
    go (Pair l r) =
      let leftPart = go l
          rightPart = go r
       in case leftPart of
            (l', True) -> (Pair l' r, True)
            _ ->
              case rightPart of
                (r', True) -> (Pair l r', True)
                _ -> (Pair l r, False)
    go lf@(Leaf x)
      | x >= 10 = (Pair (Leaf (x `div` 2)) (Leaf (x - (x `div` 2))), True)
      | otherwise = (lf, False)

reduceSnailfish :: SnailFish -> SnailFish
reduceSnailfish = go
  where
    go sf =
      let explodeSnailfishdSf = explodeSnailfish sf
          splitSnailfishtedSf = splitSnailfish sf
       in if snd explodeSnailfishdSf
            then reduceSnailfish (fst explodeSnailfishdSf)
            else
              if snd splitSnailfishtedSf
                then reduceSnailfish (fst splitSnailfishtedSf)
                else sf

combineSnailfish :: SnailFish -> SnailFish -> SnailFish
combineSnailfish sf1 sf2 = reduceSnailfish (Pair sf1 sf2)

combineSnailfishes :: SnailFish -> [SnailFish] -> SnailFish
combineSnailfishes = foldl combineSnailfish

fromStringToSnailfish :: String -> Maybe SnailFish
fromStringToSnailfish input = do
  (sf, stack, rest) <- go [] input
  if checkBalanced stack rest then Just sf else Nothing
  where
    go stack ('[' : rest) = do
      (l, stack', rest') <- go ('[' : stack) rest
      (r, stack'', rest'') <- go stack' rest'
      Just (Pair l r, stack'', rest'')
    go ('[' : stack) (']' : rest) = go stack rest
    go stack (',' : '[' : xs) = go stack ('[' : xs)
    go stack (',' : x : xs) | isDigit x = go stack (x : xs)
    go stack xs@(x : _) | isDigit x = do
      let (maybeN, rest) = parseInt xs
      n <- maybeN
      case (stack, rest) of
        ('[' : stack', ']' : rest') -> Just (Leaf n, stack', rest')
        (stack, ',' : rest') -> Just (Leaf n, stack, rest')
        _ -> Nothing
    go _ _ = Nothing

    checkBalanced [] [] = True
    checkBalanced ('[' : stack) (']' : rest) = checkBalanced stack rest
    checkBalanced _ _ = False

fromStringsToSnailfishes :: [String] -> Maybe [SnailFish]
fromStringsToSnailfishes = traverse fromStringToSnailfish

solvePart1 :: String -> Maybe Int
solvePart1 input = do
  sfs <- fromStringsToSnailfishes (lines input)
  case sfs of
    (sf : sfs) -> Just $ magnitudeSnailfish $ combineSnailfishes sf sfs
    _ -> Nothing

solvePart2 :: String -> Maybe Int
solvePart2 input = do
  sfs <- fromStringsToSnailfishes (lines input)
  case sfs of
    [] -> Nothing
    sfs -> Just $ maximum [max (magnitudeSnailfish (combineSnailfish x y)) (magnitudeSnailfish (combineSnailfish y x)) | [x, y] <- subsets 2 sfs]

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  input <- readFile filename

  let part1Ans = solvePart1 input
      part2Ans = solvePart2 input

  putStrLn $ "Part1: " ++ show part1Ans
  putStrLn $ "Part2: " ++ show part2Ans

  pure ()
