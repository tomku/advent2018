module Advent where

import           Text.ParserCombinators.ReadP
import           System.FilePath
import           System.Directory
import           Data.Char

-- Accessing provided data

-- Path to data directory
dataDir :: IO FilePath
dataDir = do
    cwd <- getCurrentDirectory
    return $ cwd </> "../data/"

-- Read an entire file from the data directory
readData :: FilePath -> IO String
readData file = do
    path <- dataDir
    readFile $ path </> file

-- Parsing stuff

-- Single digit
digit :: ReadP Char
digit = satisfy isDigit

-- Plus sign
posSign :: ReadP Integer
posSign = do
    sigil <- satisfy (== '+')
    return 1

--  Minus sign
negSign :: ReadP Integer
negSign = do
    sigil <- satisfy (== '-')
    return (-1)

-- Mandatory sign followed by an integer
delta :: ReadP Integer
delta = do
    sign <- choice [negSign, posSign]
    num  <- read <$> many1 digit
    return $ sign * num
