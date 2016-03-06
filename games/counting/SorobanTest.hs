--{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}

import Soroban
import Data.Maybe
import Control.Monad
--import Control.DeepSeq
--import GHC.Generics (Generic)

--deriving instance Generic Soroban
--deriving instance NFData Soroban

checkId :: (Show a, Eq a) => (a -> a) -> a -> IO ()
checkId f x = unless  (x == f x) $ putStrLn $ "NG: " ++ show (x, f x)

checkEq :: (Show a, Eq a) => a -> a -> IO ()
checkEq x y = unless (x == y) $ putStrLn $ "NG: " ++ show (x, y)

sample0 = fromJust $ readSoroban "OOOOOOOOOO----------"
sample1 = fromJust $ readSoroban "-OOOOOOOOOO---------"
sample2 = fromJust $ readSoroban "O-OOOOO-------------"
sample3 = fromJust $ readSoroban "OOOOOOOO--O----O----"
sample4 = fromJust $ readSoroban "----------OOOOOOOOOO"
sample5 = fromJust $ readSoroban "OOOOOO----------OOOO"

-- cluster
--[(0,10)]
--[(1,10)]
--[(0,1),(2,5)]
--[(0,8),(10,1),(15,1)]
--[(10,10)]

test = do
    --putStrLn $ concat $ map show $ take 20 $ cycle [0..9]
    checkEq "OOOOOOOOOO----------" $ show sample0
    checkEq "-OOOOOOOOOO---------" $ show sample1
    checkEq "O-OOOOO-------------" $ show sample2
    checkEq "OOOOOOOO--O----O----" $ show sample3
    checkEq "----------OOOOOOOOOO" $ show sample4
    --checkId (fromJust . readSoroban . show) sample0
    --checkId (fromJust . readSoroban . show) sample1
    --checkId (fromJust . readSoroban . show) sample2
    --checkId (fromJust . readSoroban . show) sample3
    --checkId (fromJust . readSoroban . show) sample4
    checkId (chunkToSoroban 20 . sorobanToChunk) sample0
    checkId (chunkToSoroban 20 . sorobanToChunk) sample1
    checkId (chunkToSoroban 20 . sorobanToChunk) sample2
    checkId (chunkToSoroban 20 . sorobanToChunk) sample3
    checkId (chunkToSoroban 20 . sorobanToChunk) sample4

testMove = do
    --right
    checkEq "OOOOOOOOO---O-------" $ show $ fromJust $ move sample0 9 12
    checkEq "OOOOOOOO----OO------" $ show $ fromJust $ move sample0 8 12
    checkEq "OOOOOOO---OOO-------" $ show $ fromJust $ move sample0 7 10
    checkEq "OOO-------OOOOOOO---" $ show $ fromJust $ move sample0 3 10
    checkEq "O---------OOOOOOOOO-" $ show $ fromJust $ move sample0 1 10
    checkEq "----------OOOOOOOOOO" $ show $ fromJust $ move sample0 0 10
    checkEq "OOOOOOO-----OO-O----" $ show $ fromJust $ move sample3 7 12
    checkEq "----------OOOOOOOOOO" $ show $ fromJust $ move sample0 0 13
    --left
    checkEq "-------OO---OOOOOOOO" $ show $ fromJust $ move sample4 11 8
    checkEq Nothing $ move sample3 3 12
    checkEq Nothing $ move sample5 16 17
    checkEq Nothing $ move sample0 12 8
    checkEq Nothing $ move sample0 13 10

main = do
    test
    testMove

