import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

prompt :: String -> IO String
prompt l = do
    putStr l
    hFlush stdout
    getLine

pairwise :: [a] -> [(a, a)]
pairwise (x:y:xs) = (x, y) : pairwise (y:xs)
pairwise _ = []

type OasisReport = [[Int]]

parseOasisReportFromString :: String -> Maybe OasisReport
parseOasisReportFromString = traverse (traverse readMaybe . words) . lines

parseOasisReportFromFile :: String -> IO (Maybe OasisReport)
parseOasisReportFromFile = fmap parseOasisReportFromString . readFile

diffs :: Num a => [a] -> [a]
diffs = fmap (uncurry (flip (-))) . pairwise

allZeros :: (Eq a, Num a) => [a] -> Bool
allZeros = all (== 0)

extrapolateForward :: [Int] -> Int
extrapolateForward = go <$> last <*> id
  where
    go acc []  = acc
    go acc [_] = acc
    go acc xs | allZeros xs = acc
              | otherwise   = let xs' = diffs xs in go (acc + last xs') xs'

solvePart1ForOasisReport :: OasisReport -> Int
solvePart1ForOasisReport = sum . fmap extrapolateForward

solvePart1ForFile :: String -> IO Int
solvePart1ForFile filePath = do
    oasisReportMaybe <- parseOasisReportFromFile filePath
    case oasisReportMaybe of
        Nothing -> do
            putStrLn $ "Could not able to parse the file: " ++ filePath
            return (-1)
        Just oasisReport -> return $ solvePart1ForOasisReport oasisReport

extrapolateBackward :: [Int] -> Int
extrapolateBackward = go
  where
    go []  = 0
    go [_] = 0
    go xs | allZeros xs = 0
          | otherwise   = head xs - go (diffs xs)

solvePart2ForOasisReport :: OasisReport -> Int
solvePart2ForOasisReport = sum . fmap extrapolateBackward

solvePart2ForFile :: String -> IO Int
solvePart2ForFile filePath = do
    oasisReportMaybe <- parseOasisReportFromFile filePath
    case oasisReportMaybe of
        Nothing -> do
            putStrLn $ "Could not able to parse the file: " ++ filePath
            return (-1)
        Just oasisReport -> return $ solvePart2ForOasisReport oasisReport

main :: IO ()
main = do
    filePath <- prompt "Enter file path: "
    oasisReportMaybe <- parseOasisReportFromFile filePath
    case oasisReportMaybe of
        Nothing -> putStrLn $ "Could not able to parse the file: " ++ filePath
        Just oasisReport -> do
            let part1Ans = solvePart1ForOasisReport oasisReport
                part2Ans = solvePart2ForOasisReport oasisReport
            putStrLn $ "Part 1: " ++ show part1Ans
            putStrLn $ "Part 2: " ++ show part2Ans
