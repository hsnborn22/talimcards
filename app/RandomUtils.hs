{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module RandomUtils (shuffle, countNonZeroValues, getCompletionRate, getCompletionRateSingle) where

import Data.Array.IO
import qualified Data.Map as Map
import System.Random (randomRIO, newStdGen, getStdGen, randomR, RandomGen, StdGen)
import Control.Monad (foldM, forM)

-- | This function counts the number of non-zero values in a Map object.
countNonZeroValues :: Map.Map String Int -> Int
countNonZeroValues m = length $ filter (> 0) $ Map.elems m

-- | This function gives the percentage of completion of the session 
getCompletionRate :: Map.Map String Int -> Map.Map String Int -> Float
getCompletionRate m1 m2 = fromIntegral (countNonZeroValues m1 + countNonZeroValues m2) / fromIntegral (2 * length (Map.elems m1))

-- | This function gives the percentage of completion of the session 
getCompletionRateSingle :: Map.Map String Int -> Float
getCompletionRateSingle m = fromIntegral (countNonZeroValues m) / fromIntegral (length (Map.elems m))

-- | Randomly shuffle a list
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

