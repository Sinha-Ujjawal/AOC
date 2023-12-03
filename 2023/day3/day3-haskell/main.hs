import System.IO (hFlush, stdout)
import qualified Data.Array as A
import qualified Data.List as L
import Data.Array ((!))
import Data.Char (isDigit, digitToInt)
import Control.Applicative ((<|>))

prompt :: String -> IO String
prompt line = do
    putStr line
    hFlush stdout
    getLine

enumerate :: Int -> [a] -> [(Int, a)]
enumerate start = zip [start..]

type EngineSchematic = A.Array (Int, Int) Char
type PartNumber = (Char, (Int, Int), Int)

engineSchematicFromString :: String -> EngineSchematic
engineSchematicFromString text =
    let textAsLines = lines text
        height      = length textAsLines
        width       = if height == 0 then 0 else length $ head textAsLines
    in
        A.array ((0, 0), (height, width)) [
            ((i, j), char)
            | (i, line) <- enumerate 0 textAsLines
            , (j, char) <- enumerate 0 line
        ]

engineSchematicFromFile :: String -> IO EngineSchematic
engineSchematicFromFile = fmap engineSchematicFromString . readFile

extractPartNumbersFromEngineSchematic :: EngineSchematic -> [PartNumber]
extractPartNumbersFromEngineSchematic array = go [] Nothing 0 0 0
  where
    (_, (height, width)) = A.bounds array

    neighborHood :: Int -> Int -> [(Int, Int)]
    neighborHood i j =
        [
            (i+offi, j+offj)
            | offi <- [-1, 0, 1]
            , offj <- [-1, 0, 1]
            , (offi, offj) /= (0, 0)
            , i+offi >= 0 && i+offi < height
            , j+offj >= 0 && j+offj < width
        ]

    isSymbol idx = let char = array ! idx
                   in not (isDigit char) && char /= '.'

    symbolInNeighbourHood i j =
        case filter isSymbol $ neighborHood i j of
            (idx:_) -> Just (array ! idx, idx)
            _     -> Nothing

    go acc nearestSymbolMaybe currentNum i j
      | i >= height = acc
      | j >= width  = let acc' = case nearestSymbolMaybe of
                            Just (sym, idx) -> (sym, idx, currentNum):acc
                            Nothing         -> acc
                      in go acc' Nothing 0 (i+1) 0
      | otherwise   = let char = array ! (i, j)
                      in
                        if isDigit char then
                            go acc (nearestSymbolMaybe <|> symbolInNeighbourHood i j) (currentNum*10 + digitToInt char) i (j+1)
                        else
                            let acc' = case nearestSymbolMaybe of
                                    Just (sym, idx) -> (sym, idx, currentNum):acc
                                    Nothing         -> acc
                            in go acc' Nothing 0 i (j+1)

solvePart1ForInput :: EngineSchematic -> Int
solvePart1ForInput = sum . fmap (\(_, _, n) -> n) . extractPartNumbersFromEngineSchematic

solvePart1ForFile :: String -> IO Int
solvePart1ForFile = fmap solvePart1ForInput . engineSchematicFromFile

solvePart2ForInput :: EngineSchematic -> Int
solvePart2ForInput = sum . gearRatios . extractPartNumbersFromEngineSchematic
  where
    gearRatios :: [PartNumber] -> [Int]
    gearRatios =
        fmap (\[(_, _, n1), (_, _, n2)] -> n1*n2)
        . filter ((== 2) . length)
        . L.groupBy (\(_, idx1, _) (_, idx2, _) -> idx1 == idx2)
        . L.sortOn (\(_, idx, _) -> idx)
        . filter (\(c, _, _) -> c == '*')

solvePart2ForFile :: String -> IO Int
solvePart2ForFile = fmap solvePart2ForInput . engineSchematicFromFile

main :: IO ()
main = do
    filePath <- prompt "Enter file path: "
    fileContent <- readFile filePath
    let es       = engineSchematicFromString fileContent
        part1Ans = solvePart1ForInput es
        part2Ans = solvePart2ForInput es
    putStrLn $ "Part 1: " ++ show part1Ans
    putStrLn $ "Part 2: " ++ show part2Ans
