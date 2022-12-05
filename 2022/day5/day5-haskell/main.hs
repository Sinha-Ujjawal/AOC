{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

pushMany :: [a] -> [a] -> [a]
pushMany xs ys = L.foldl' (flip (:)) ys xs

data Move = Move {qty :: !Int, from :: !Int, to :: !Int} deriving (Show, Eq)

type Crate = Char

data StackProgram = StackProgram {stacks :: !(M.Map Int [Crate]), instructions :: ![Move]}
  deriving (Show)

type MoveEngine = Move -> M.Map Int [Crate] -> M.Map Int [Crate]

topStack :: StackProgram -> [Char]
topStack = topStack' . stacks
  where
    topStack' :: M.Map Int [Crate] -> [Crate]
    topStack' = mapMaybe safeHead . M.elems

step :: MoveEngine -> StackProgram -> StackProgram
step applyMove sp@StackProgram {stacks, instructions} =
  case instructions of
    [] -> sp
    (move : rest) -> StackProgram {stacks = applyMove move stacks, instructions = rest}

execute :: MoveEngine -> StackProgram -> StackProgram
execute applyMove = head . dropWhile (([] /=) . instructions) . iterate (step applyMove)

parseProgram :: T.Text -> Maybe StackProgram
parseProgram = beginParsing . T.splitOn "\n\n"
  where
    beginParsing :: [T.Text] -> Maybe StackProgram
    beginParsing [layoutStr, instructionsStr] = do
      stacks <- parseLayout layoutStr
      instructions <- parseInstructions instructionsStr
      return $ StackProgram {stacks, instructions}
    beginParsing _ = Nothing

    parseLayout :: T.Text -> Maybe (M.Map Int [Crate])
    parseLayout layoutStr = do
      cratesStrs <- safeInit $ T.lines layoutStr
      crates <- traverse parseCrate cratesStrs
      return $ buildStack crates

    buildStack :: [[Maybe Crate]] -> M.Map Int [Crate]
    buildStack = L.foldl' (M.unionWith (++)) M.empty . fmap buildStack'

    buildStack' :: [Maybe Crate] -> M.Map Int [Crate]
    buildStack' = L.foldl' buildStack'' M.empty . zip [1 ..]

    buildStack'' :: M.Map Int [Crate] -> (Int, Maybe Crate) -> M.Map Int [Crate]
    buildStack'' accum (_, Nothing) = accum
    buildStack'' accum (idx, Just crate) = M.insert idx [crate] accum

    parseCrate :: T.Text -> Maybe [Maybe Crate]
    parseCrate "" = Just [Nothing]
    parseCrate (T.splitAt 3 -> (crateStr, restStr)) = do
      crateMaybe <- parseCrate' crateStr

      restCrateMaybes <-
        case T.uncons restStr of
          Just (' ', restStr) -> parseCrate restStr
          Just (_, _) -> Nothing
          Nothing -> return []
      return $ crateMaybe : restCrateMaybes

    parseCrate' :: T.Text -> Maybe (Maybe Crate)
    parseCrate' (T.unpack -> ['[', crate, ']']) = Just (Just crate)
    parseCrate' "   " = Just Nothing
    parseCrate' _ = Nothing

    parseInstructions' :: T.Text -> Maybe Move
    parseInstructions' (T.stripPrefix "move " -> Just rest) = parseInstructions'' rest
    parseInstructions' _ = Nothing

    parseInstructions :: T.Text -> Maybe [Move]
    parseInstructions = traverse parseInstructions' . T.lines

    parseInstructions'' :: T.Text -> Maybe Move
    parseInstructions'' (T.splitOn " from " -> [qtyStr, fromToStr]) = do
      qty <- parseQty qtyStr
      (from, to) <- parseFromTo fromToStr
      return Move {qty, from, to}
    parseInstructions'' _ = Nothing

    parseQty :: T.Text -> Maybe Int
    parseQty = readMaybe . T.unpack

    parseFromTo :: T.Text -> Maybe (Int, Int)
    parseFromTo (T.splitOn " to " -> [fromStr, toStr]) = do
      from <- readMaybe $ T.unpack fromStr
      to <- readMaybe $ T.unpack toStr
      return (from, to)
    parseFromTo _ = Nothing

solvePart1 :: StackProgram -> [Crate]
solvePart1 = topStack . execute applyMove
  where
    applyMove :: MoveEngine
    applyMove Move {qty, from, to} stacks = M.insert from fromStack' (M.insert to toStack' stacks)
      where
        fromStack = fromMaybe [] $ M.lookup from stacks
        toStack = fromMaybe [] $ M.lookup to stacks
        (items, fromStack') = L.splitAt qty fromStack
        toStack' = pushMany items toStack

solvePart2 :: StackProgram -> [Crate]
solvePart2 = topStack . execute applyMove
  where
    applyMove :: MoveEngine
    applyMove Move {qty, from, to} stacks = M.insert from fromStack' (M.insert to toStack' stacks)
      where
        fromStack = fromMaybe [] $ M.lookup from stacks
        toStack = fromMaybe [] $ M.lookup to stacks
        (items, fromStack') = L.splitAt qty fromStack
        toStack' = items ++ toStack

main :: IO ()
main = do
  filePath <- prompt "Enter file path: "
  contents <- T.stripEnd . T.pack <$> readFile filePath
  case parseProgram contents of
    Nothing -> error "Cannot parse the file as a Stack Program!"
    Just stackProgram -> do
      let part1Ans = solvePart1 stackProgram
      putStrLn $ "Part 1: " ++ part1Ans
      let part2Ans = solvePart2 stackProgram
      putStrLn $ "Part 2: " ++ part2Ans
