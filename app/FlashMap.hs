{-# LANGUAGE TemplateHaskell #-}
module FlashMap (trim, trimQuotes, parseCommaPairs, parseDate, parseDates, writeCommaPairs, localTimeToString, convertDateMapToStrMap, writeDatePairs, parseKnowledgePairs, parseDatePairs, changeKey) where

import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Lens.Micro.TH
import Data.Time
import Data.Time.Format (defaultTimeLocale)
import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import System.IO (withFile, hClose, readFile, IOMode(WriteMode), hPutStrLn,openFile ,IOMode(ReadMode), hGetContents)

-- trim function: this function removes all the spaces at the beginning and the end of a string
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

trimQuotes :: String -> String
trimQuotes = reverse . dropWhile (== '"') . reverse . dropWhile (== '"')

{- |This function takes in a string (the content of the file containing the 
 - key-value pairs of the flashcard set) and returns a Map structure encoding the
 - flashcard set. -}
parseCommaPairs :: String -> Map.Map String String
parseCommaPairs s = Map.fromList $ map (\[a, b] -> (trim a, trim b)) 
                                     (filter (\x -> length x == 2) splitPairs)
  where
    splitPairs = map (splitOn ",") (splitOn "\n" s)

parseDate :: String -> Maybe LocalTime
parseDate str = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" str

{- |This function parses the file containing keys and the revision time, encoded in
 - a specific date format -}
parseDates :: String -> Map.Map String (Maybe LocalTime)
parseDates s = Map.map parseDate (parseCommaPairs s)

writeCommaPairs :: (Show a) => Map.Map String a -> String -> IO ()
writeCommaPairs m fName = do
  handle <- openFile fName WriteMode

  _ <- Map.traverseWithKey (\key value -> do
    hPutStrLn handle $ key ++ "," ++ (trimQuotes(show value))) m 
  hClose handle -- Close the file handle

localTimeToString :: Maybe LocalTime -> String
localTimeToString Nothing = "Nothing"
localTimeToString (Just localTime) = trimQuotes (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" localTime)

convertDateMapToStrMap :: Map.Map String (Maybe LocalTime) -> Map.Map String String
convertDateMapToStrMap m = Map.map localTimeToString m

writeDatePairs :: Map.Map String (Maybe LocalTime) -> String -> IO ()
writeDatePairs m fName = writeCommaPairs (convertDateMapToStrMap m) fName

-- When we are going to parse the knowledge levels file, we will add Map.map (\x -> read x :: Int) in front of the
-- output of the parseCommaPairs function, i.e. we'll convert each value of a key-value pair into its int representation.
parseKnowledgePairs :: String -> Map.Map String String -> Map.Map String Int
parseKnowledgePairs s m = finalMap
  where
    -- Step 1: Parse the string into a map
    parsedMap = Map.map (\x -> read x :: Int) (parseCommaPairs s)

    -- Step 2: Get keys from the input map `m`
    keysInM = Map.keys m

    -- Step 3: Identify keys in `m` not present in `parsedMap`
    missingKeys = filter (`Map.notMember` parsedMap) keysInM

    -- Step 4: Add missing keys with default value 0
    defaultMap = Map.fromList [(key, 0) | key <- missingKeys]

    -- Combine parsedMap with defaultMap
    finalMap = Map.union parsedMap defaultMap

parseDatePairs :: String -> Map.Map String String -> Map.Map String (Maybe LocalTime)
parseDatePairs s m = finalMap
  where
    -- Step 1: Parse the string into a map
    parsedMap = parseDates s

    -- Step 2: Get keys from the input map `m`
    keysInM = Map.keys m

    -- Step 3: Identify keys in `m` not present in `parsedMap`
    missingKeys = filter (`Map.notMember` parsedMap) keysInM

    -- Step 4: Add missing keys with default value 0
    defaultMap = Map.fromList [(key, Nothing) | key <- missingKeys]

    -- Combine parsedMap with defaultMap
    finalMap = Map.union parsedMap defaultMap

changeKey :: Ord k => k -> k -> Map.Map k v -> Map.Map k v
changeKey oldKey newKey m = 
    case Map.lookup oldKey m of
        Just value -> Map.insert newKey value (Map.delete oldKey m)
        Nothing -> m

