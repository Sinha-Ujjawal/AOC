{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Applicative (Alternative (..))
import Data.Bits (Bits (complement, shiftL, shiftR, (.&.), (.|.)))
import qualified Data.Char as Char
import Data.Functor.Identity (Identity)
import qualified Data.HashTable.IO as H
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Word (Word16)
import qualified System.Environment as SE
import System.IO (hFlush, stdout)
import qualified Text.Read as T

type HashTable k v = H.BasicHashTable k v

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

type Wire = T.Text

type Value = Word16

data Probe = WireType !Wire | ValueType !Value deriving (Show, Eq)

data Instruction
  = CopyFrom !Probe !Wire
  | AndGate !Probe !Probe !Wire
  | OrGate !Probe !Probe !Wire
  | NotGate !Probe !Wire
  | LShift !Probe !Probe !Wire
  | RShift !Probe !Probe !Wire
  deriving (Show, Eq)

wire :: Instruction -> Wire
wire (CopyFrom _ w) = w
wire (AndGate _ _ w) = w
wire (OrGate _ _ w) = w
wire (NotGate _ w) = w
wire (LShift _ _ w) = w
wire (RShift _ _ w) = w

collectWires :: [Instruction] -> IO (HashTable Wire Instruction)
collectWires = H.fromList . fmap (\ins -> (wire ins, ins))

evaluateAt :: Wire -> [Instruction] -> IO (Maybe Value)
evaluateAt w instructions = do
  wireToInstructionMap <- collectWires instructions
  memo :: HashTable Wire Value <- H.new
  evalWire wireToInstructionMap memo w
  where
    evalProbe :: HashTable Wire Instruction -> HashTable Wire Value -> Probe -> IO (Maybe Value)
    evalProbe wireToInstructionMap memo (WireType w) = evalWire wireToInstructionMap memo w
    evalProbe _ _ (ValueType v) = return (Just v)

    maybeAnd :: Maybe Value -> Maybe Value -> Maybe Value
    maybeAnd (Just v1) (Just v2) = Just (v1 .&. v2)
    maybeAnd _ _ = Nothing

    maybeOr :: Maybe Value -> Maybe Value -> Maybe Value
    maybeOr (Just v1) (Just v2) = Just (v1 .|. v2)
    maybeOr _ _ = Nothing

    maybeComplement :: Maybe Value -> Maybe Value
    maybeComplement = fmap complement

    maybeLShift :: Maybe Value -> Maybe Value -> Maybe Value
    maybeLShift (Just v1) (Just v2) = Just (v1 `shiftL` fromIntegral v2)
    maybeLShift _ _ = Nothing

    maybeRShift :: Maybe Value -> Maybe Value -> Maybe Value
    maybeRShift (Just v1) (Just v2) = Just (v1 `shiftR` fromIntegral v2)
    maybeRShift _ _ = Nothing

    evalWire :: HashTable Wire Instruction -> HashTable Wire Value -> Wire -> IO (Maybe Value)
    evalWire wireToInstructionMap memo w = do
      maybeVal <- H.lookup memo w
      case maybeVal of
        Just _ -> return maybeVal
        Nothing -> do
          maybeIns <- H.lookup wireToInstructionMap w
          maybeVal <-
            case maybeIns of
              Nothing -> return Nothing
              Just (CopyFrom p _) -> evalProbe wireToInstructionMap memo p
              Just (AndGate p1 p2 _) -> do
                maybeV1 <- evalProbe wireToInstructionMap memo p1
                maybeV2 <- evalProbe wireToInstructionMap memo p2
                return $ maybeAnd maybeV1 maybeV2
              Just (OrGate p1 p2 _) -> do
                maybeV1 <- evalProbe wireToInstructionMap memo p1
                maybeV2 <- evalProbe wireToInstructionMap memo p2
                return $ maybeOr maybeV1 maybeV2
              Just (NotGate p _) -> maybeComplement <$> evalProbe wireToInstructionMap memo p
              Just (LShift p1 p2 _) -> do
                maybeV1 <- evalProbe wireToInstructionMap memo p1
                maybeV2 <- evalProbe wireToInstructionMap memo p2
                return $ maybeLShift maybeV1 maybeV2
              Just (RShift p1 p2 _) -> do
                maybeV1 <- evalProbe wireToInstructionMap memo p1
                maybeV2 <- evalProbe wireToInstructionMap memo p2
                return $ maybeRShift maybeV1 maybeV2
          case maybeVal of
            Just val -> do
              H.insert memo w val
              return maybeVal
            Nothing -> return Nothing

parseProbe :: T.Text -> Maybe Probe
parseProbe p
  | T.all Char.isLower p = Just $ WireType p
  | otherwise = ValueType <$> T.readMaybe (T.unpack p)

parseInstruction :: T.Text -> T.Text -> Maybe Instruction
parseInstruction (T.stripPrefix "NOT " -> Just rest) out = (`NotGate` out) <$> parseProbe rest
parseInstruction (T.splitOn " AND " -> [probe1, probe2]) out = do
  probe1 <- parseProbe probe1
  probe2 <- parseProbe probe2
  return $ AndGate probe1 probe2 out
parseInstruction (T.splitOn " OR " -> [probe1, probe2]) out = do
  probe1 <- parseProbe probe1
  probe2 <- parseProbe probe2
  return $ OrGate probe1 probe2 out
parseInstruction (T.splitOn " LSHIFT " -> [probe1, probe2]) out = do
  probe1 <- parseProbe probe1
  probe2 <- parseProbe probe2
  return $ LShift probe1 probe2 out
parseInstruction (T.splitOn " RSHIFT " -> [probe1, probe2]) out = do
  probe1 <- parseProbe probe1
  probe2 <- parseProbe probe2
  return $ RShift probe1 probe2 out
parseInstruction p out = (`CopyFrom` out) <$> parseProbe p

parseLine :: T.Text -> Maybe Instruction
parseLine (T.splitOn " -> " -> [instructionInputs, output]) = parseInstruction instructionInputs output
parseLine _ = Nothing

parseLines :: [T.Text] -> Maybe [Instruction]
parseLines = traverse parseLine

solvePart1 :: Wire -> [Instruction] -> IO Value
solvePart1 w = fmap (fromMaybe 0) . evaluateAt w

main :: IO ()
main = do
  args <- SE.getArgs
  filename <-
    case args of
      (filename : _) -> return filename
      _otherwise -> prompt "Enter file name: "
  lines <- T.lines . T.pack <$> readFile filename
  instructions <-
    case parseLines lines of
      Just instructions -> return instructions
      Nothing -> error "Cannot parse instructions from the file"

  w <- T.pack <$> prompt "Evalulate at which wire? "
  part1Ans <- solvePart1 w instructions
  putStrLn $ "Part 1: " ++ show part1Ans

  ow <- T.pack <$> prompt ("Override which wire with value " ++ show part1Ans ++ "? ")
  let instructions' = filter (\ins -> wire ins /= ow) instructions ++ [CopyFrom (ValueType part1Ans) ow]
  part2Ans <- solvePart1 w instructions'
  putStrLn $ "Part 2: " ++ show part2Ans
