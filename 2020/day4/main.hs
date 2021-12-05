{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Char (isAlphaNum, isDigit)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Text as T
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

readMaybeTxt :: Read a => T.Text -> Maybe a
readMaybeTxt = readMaybe . T.unpack

parseKeyValueMaybe :: T.Text -> Maybe (T.Text, T.Text)
parseKeyValueMaybe = go . T.splitOn ":"
  where
    go [key, value] = Just (key, value)
    go _ = Nothing

data HairColor = AMB | BLU | BRN | GRY | GRN | HZL | OTH deriving (Show, Eq)

data LengthUnit = CM | IN deriving (Show, Eq)

data Passport = Passport
  { byr :: Int,
    iyr :: Int,
    eyr :: Int,
    hgt :: (Int, LengthUnit),
    hcl :: T.Text,
    ecl :: HairColor,
    pid :: Int,
    cid :: Maybe T.Text
  }
  deriving (Show, Eq)

parsePassport :: T.Text -> [(T.Text, T.Text)]
parsePassport = mapMaybe parseKeyValueMaybe . T.words

allKeysPresent :: [(T.Text, T.Text)] -> Bool
allKeysPresent passportInfo =
  all (isJust . flip lookup passportInfo) keys
  where
    keys =
      map
        T.pack
        [ "byr",
          "iyr",
          "eyr",
          "hgt",
          "hcl",
          "ecl",
          "pid"
        ]

mkValidPassportMaybe :: [(T.Text, T.Text)] -> Maybe Passport
mkValidPassportMaybe passportInfo =
  do
    byr <- extractByr passportInfo
    iyr <- extractIyr passportInfo
    eyr <- extractEyr passportInfo
    hgt <- extractHgt passportInfo
    hcl <- extractHcl passportInfo
    ecl <- extractEcl passportInfo
    pid <- extractPid passportInfo
    cid <- extractCid passportInfo
    pure $ Passport byr iyr eyr hgt hcl ecl pid cid
  where
    extractYear lb ub key passportInfo = do
      yrTxt <- lookup key passportInfo
      yr <- readMaybeTxt yrTxt
      if yr >= lb && yr <= ub
        then Just yr
        else Nothing

    extractByr = extractYear 1920 2002 "byr"
    extractIyr = extractYear 2010 2020 "iyr"
    extractEyr = extractYear 2020 2030 "eyr"

    extractHgt passportInfo = do
      hgt <- lookup "hgt" passportInfo
      case ((,) <$> readMaybeTxt . fst <*> snd) $ T.splitAt (T.length hgt - 2) hgt of
        (Just hgtValue, "cm") ->
          if hgtValue >= 150 && hgtValue <= 193
            then Just (hgtValue, CM)
            else Nothing
        (Just hgtValue, "in") ->
          if hgtValue >= 59 && hgtValue <= 76
            then Just (hgtValue, IN)
            else Nothing
        _ -> Nothing

    extractHcl passportInfo = do
      hcl <- lookup "hcl" passportInfo
      case hcl of
        (T.unpack -> '#' : rest) -> if all isAlphaNum rest then Just hcl else Nothing
        _ -> Nothing

    extractEcl passportInfo = do
      ecl <- lookup "ecl" passportInfo
      case T.strip ecl of
        "amb" -> Just AMB
        "blu" -> Just BLU
        "brn" -> Just BRN
        "gry" -> Just GRY
        "grn" -> Just GRN
        "hzl" -> Just HZL
        "oth" -> Just OTH
        _ -> Nothing

    extractPid passportInfo = do
      pid <- T.strip <$> lookup "pid" passportInfo
      if T.length pid == 9 && all isDigit (T.unpack pid)
        then Just $ read $ T.unpack pid
        else Nothing

    extractCid = Just . lookup "cid"

parsePassports :: String -> [[(T.Text, T.Text)]]
parsePassports = map parsePassport . T.splitOn "\n\n" . T.pack

solvePart1 :: String -> Int
solvePart1 = length . filter allKeysPresent . parsePassports

solvePart2 :: String -> Int
solvePart2 = length . mapMaybe mkValidPassportMaybe . parsePassports

main :: IO ()
main = do
  filename <- prompt "Enter file name: "
  fileContent <- readFile filename

  let part1Ans = solvePart1 fileContent
      part2Ans = solvePart2 fileContent

  putStrLn $ "Part1: " ++ show part1Ans
  putStrLn $ "Part2: " ++ show part2Ans
