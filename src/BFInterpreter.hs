module BFInterpreter (exec) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Either
import Data.Int
import Data.Map.Strict as M
import Data.Word (Word8 (..))
import Debug.Trace
import Parser
import Util

data BFCommand = IncrPoint | DecrPoint | IncrVal | DecrVal | Output | Input | Loop [BFCommand] deriving Show

data Tape = Tape { 
    pointer :: Int
  , array :: M.Map Int Word8
  , input :: [Word8] } deriving Show

tape0 = Tape {
    pointer = 1
  , array = M.empty
  , input = [] }

type BFRunner = State Tape String

parseCommand :: Parser BFCommand
parseCommand = 
      parseIncrPoint
  <|> parseDecrPoint
  <|> parseIncrVal
  <|> parseDecrVal
  <|> parseOutput
  <|> parseInput
  <|> parseLoop

parseIncrPoint, parseDecrPoint, parseIncrVal, parseDecrVal, parseOutput, parseInput, parseLoop :: Parser BFCommand
parseIncrPoint = IncrPoint <$ char '>'

parseDecrPoint = DecrPoint <$ char '<'

parseIncrVal = IncrVal <$ char '+'

parseDecrVal = DecrVal <$ char '-'

parseOutput = Output <$ char '.'

parseInput = Input <$ char ','

parseLoop = do
  char '['
  xs <- many parseCommand
  char ']'
  return $ Loop xs

runCommand :: BFCommand -> BFRunner
runCommand IncrPoint = (modify $ \s -> s { pointer = (pointer s) + 1 }) >> return mempty
runCommand DecrPoint = (modify $ \s -> s { pointer = (pointer s) - 1 }) >> return mempty
runCommand IncrVal = (modify $ \s -> s { array = if (pointer s) `member` (array s) then M.adjust (+1) (pointer s) (array s) else M.insert (pointer s) 1 (array s) }) >> return mempty
runCommand DecrVal = (modify $ \s -> s { array = if (pointer s) `member` (array s) then M.adjust (subtract 1) (pointer s) (array s) else M.insert (pointer s) (-1) (array s) }) >> return mempty
runCommand Output = get >>= \s -> return $ charToC $ M.findWithDefault 0 (pointer s) (array s)
runCommand Input = (modify $ \s -> s { array = M.insert (pointer s) (head $ input s) (array s), input = tail (input s)}) >> return mempty
runCommand (Loop cmds) = do
  s <- get
  when (M.findWithDefault 0 (pointer s) (array s) /= 0) $
    mapM_ runCommand cmds >> (void $ runCommand $ Loop cmds)
  return mempty

exec :: String -> String
exec prg = case parse (many parseCommand) prg of
  Right cmds -> concat $ evalState (mapM runCommand cmds) tape0
  Left str -> str
