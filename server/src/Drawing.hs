module Drawing (selectWinners) where

import System.Random
import Control.Monad
import Control.Arrow

--Random Number Generator
randomNumGen :: Random a => a -> a -> IO a
randomNumGen min max = do
    g <- newStdGen   
    pure ( fst $ randomR (min,max) g)

--Takes a list, Returns a random number from length of list.
drawWinningNumber :: [a] -> IO Int
drawWinningNumber list = randomNumGen 1 (length list)

getWinner :: [a] -> IO a
getWinner list = fmap (\x -> list !! (x - 1)) (drawWinningNumber list)
--    xs !! n finds the nth element in list xs

--sequence turns the (a, IO b) into IO (a,b)
selectWinners :: [(a,[b])] -> IO [(a,b)]
selectWinners = sequence . fmap (sequence . second getWinner)