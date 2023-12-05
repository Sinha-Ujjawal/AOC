{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

import System.IO (hFlush, stdout)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as M
import Data.Char (isSpace)
import Text.Read (readMaybe)
import Control.Monad((<=<))

prompt :: String -> IO String
prompt l = do
    putStr l
    hFlush stdout
    getLine

data Almanac
    = Almanac {
        seeds                    :: [Int],
        seedToSoilMap            :: M.Map Int (Int, Int),
        soilToFertilizerMap      :: M.Map Int (Int, Int),
        fertilizerToWaterMap     :: M.Map Int (Int, Int),
        waterToLightMap          :: M.Map Int (Int, Int),
        lightToTemperatureMap    :: M.Map Int (Int, Int),
        temperatureToHumidityMap :: M.Map Int (Int, Int),
        humidityToLocationMap    :: M.Map Int (Int, Int)
    }
    deriving (Show, Eq)

applyMap :: M.Map Int (Int, Int) -> Int -> Int
applyMap m x =
    case M.lookupLE x m of
        Nothing -> x
        Just (src, (dest, n)) ->
            if x < src+n then
                dest + x - src
            else
                x

applyMapRange :: M.Map Int (Int, Int) -> (Int, Int) -> [(Int, Int)]
applyMapRange m (start, end) =
    if start > end then
        []
    else
        case M.lookupLE start m of
            Nothing -> [(start, end)]
            Just (src, (dest, n)) ->
                if start < src+n then
                    let end' = min end (src+n-1)
                    in (dest + start - src, dest + end' - src) : applyMapRange m (end'+1, end)
                else
                    [(start, end)]

seedLocation :: Almanac -> Int -> Int
seedLocation
    Almanac {
        seedToSoilMap,
        soilToFertilizerMap,
        fertilizerToWaterMap,
        waterToLightMap,
        lightToTemperatureMap,
        temperatureToHumidityMap,
        humidityToLocationMap
    } =
        applyMap humidityToLocationMap
        . applyMap temperatureToHumidityMap
        . applyMap lightToTemperatureMap
        . applyMap waterToLightMap
        . applyMap fertilizerToWaterMap
        . applyMap soilToFertilizerMap
        . applyMap seedToSoilMap

seedLocations :: Almanac -> (Int, Int) -> [(Int, Int)]
seedLocations
    Almanac {
        seedToSoilMap,
        soilToFertilizerMap,
        fertilizerToWaterMap,
        waterToLightMap,
        lightToTemperatureMap,
        temperatureToHumidityMap,
        humidityToLocationMap
    } =
        applyMapRange humidityToLocationMap
        <=< applyMapRange temperatureToHumidityMap
        <=< applyMapRange lightToTemperatureMap
        <=< applyMapRange waterToLightMap
        <=< applyMapRange fertilizerToWaterMap
        <=< applyMapRange soilToFertilizerMap
        <=< applyMapRange seedToSoilMap

parseNumbers :: T.Text -> Maybe [Int]
parseNumbers = traverse (readMaybe . T.unpack) . filter (/= "") . T.split isSpace

parseAlmanacFromString :: T.Text -> Maybe Almanac
parseAlmanacFromString text = do
    case T.splitOn "\n\n" text of
        [ seedsStr
         , T.stripPrefix "seed-to-soil map:\n"            -> Just seedToSoilMapStr
         , T.stripPrefix "soil-to-fertilizer map:\n"      -> Just soilToFertilizerMapStr
         , T.stripPrefix "fertilizer-to-water map:\n"     -> Just fertilizerToWaterMapStr
         , T.stripPrefix "water-to-light map:\n"          -> Just waterToLightMapStr
         , T.stripPrefix "light-to-temperature map:\n"    -> Just lightToTemperatureMapStr
         , T.stripPrefix "temperature-to-humidity map:\n" -> Just temperatureToHumidityMapStr
         , T.stripPrefix "humidity-to-location map:\n"    -> Just humidityToLocationMapStr ] -> do
            seeds                    <- parseSeeds seedsStr
            seedToSoilMap            <- parseMap seedToSoilMapStr
            soilToFertilizerMap      <- parseMap soilToFertilizerMapStr
            fertilizerToWaterMap     <- parseMap fertilizerToWaterMapStr
            waterToLightMap          <- parseMap waterToLightMapStr
            lightToTemperatureMap    <- parseMap lightToTemperatureMapStr
            temperatureToHumidityMap <- parseMap temperatureToHumidityMapStr
            humidityToLocationMap    <- parseMap humidityToLocationMapStr
            return Almanac {
                seeds                    = seeds,
                seedToSoilMap            = seedToSoilMap,
                soilToFertilizerMap      = soilToFertilizerMap,
                fertilizerToWaterMap     = fertilizerToWaterMap,
                waterToLightMap          = waterToLightMap,
                lightToTemperatureMap    = lightToTemperatureMap,
                temperatureToHumidityMap = temperatureToHumidityMap,
                humidityToLocationMap    = humidityToLocationMap
            }
        _ ->
            Nothing
    where
        parseSeeds :: T.Text -> Maybe [Int]
        parseSeeds = parseNumbers <=< T.stripPrefix "seeds: "

        extractRange :: [Int] -> Maybe (Int, Int, Int)
        extractRange [x, y, z] = Just (x, y, z)
        extractRange _         = Nothing

        parseMapData :: T.Text -> Maybe [(Int, Int, Int)]
        parseMapData = sequenceA <=< (traverse (traverse extractRange . parseNumbers) . T.lines)

        parseMap :: T.Text -> Maybe (M.Map Int (Int, Int))
        parseMap = fmap (M.fromList . fmap (\(dest, src, n) -> (src, (dest, n)))) . parseMapData

parseAlmanacFromFile :: String -> IO (Maybe Almanac)
parseAlmanacFromFile = fmap parseAlmanacFromString . T.readFile

solvePart1ForInput :: Almanac -> Int
solvePart1ForInput alm = minimum . fmap (seedLocation alm) $ seeds alm

solvePart1ForFile :: String -> IO Int
solvePart1ForFile filePath = do
    almMaybe <- parseAlmanacFromFile filePath
    case almMaybe of
        Nothing -> do
            putStrLn $ "Could not able to parse file: " ++ filePath
            return (-1)
        Just alm -> return $ solvePart1ForInput alm

solvePart2ForInput :: Almanac -> Int
solvePart2ForInput alm =
    minimumLocationFromSeedRanges 99999999
    $ seeds alm
  where
    minimumLocationFromSeedRanges :: Int -> [Int] -> Int
    minimumLocationFromSeedRanges acc (start : n : rest) = minimumLocationFromSeedRanges (minimum (fst <$> seedLocations alm (start, start+n)) `min`  acc) rest
    minimumLocationFromSeedRanges acc _                            = acc

solvePart2ForFile :: String -> IO Int
solvePart2ForFile filePath = do
    almMaybe <- parseAlmanacFromFile filePath
    case almMaybe of
        Nothing -> do
            putStrLn $ "Could not able to parse file: " ++ filePath
            return (-1)
        Just alm -> return $ solvePart2ForInput alm

main :: IO ()
main = do
    filePath <- prompt "Enter file path: "
    almMaybe <- parseAlmanacFromFile filePath
    case almMaybe of
        Nothing -> putStrLn $ "Could not able to parse file: " ++ filePath
        Just alm -> do
            let part1Ans = solvePart1ForInput alm
                part2Ans = solvePart2ForInput alm
            putStrLn $ "Part 1: " ++ show part1Ans
            putStrLn $ "Part 2: " ++ show part2Ans
