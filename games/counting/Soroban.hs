{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE CPP #-}
--{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Soroban
    ( Soroban(..)
    , Position
    , Size
    , Chunk
#ifndef __HASTE__
    , readSoroban
#endif
    , sorobanToChunk
    , chunkToSoroban
    , move
    ) where

import Debug.Trace
import Data.List
import Data.Maybe
import Control.Monad
--import Control.Arrow
#ifndef __HASTE__
import Text.Parsec
#endif
--import Control.DeepSeq
--import GHC.Generics (Generic)
import Util

debugShow :: (Show b) => b -> c -> c
--debugShow = traceShow
debugShow = flip const

type Position = Int
type Size     = Int
type Chunk = (Position, Size)

-- spec of balls: idx1 < idx2 <=> pPos1 < pPos2
data Soroban = Soroban {
  numBall :: Size,
  balls :: [Position]
} deriving (Eq)

instance Show Soroban where
    show (Soroban n xs) = show' 0 xs where
        show' i ps'@(p:ps) = if i == p
                                then 'O':show' (i+1) ps
                                else '-':show' (i+1) ps'
        show' i [] = replicate (n-i) '-'

#ifndef __HASTE__
readSoroban :: String -> Maybe Soroban
readSoroban s = eitherToMaybe $ do
        s' <- runP  (many $ oneOf "-O") 0 "" s
        let xs = map fst $ filter (\(x,y) -> y == 'O')  (zip [0..] s')
        return $ Soroban (length s') xs
        where
            eitherToMaybe = either (const Nothing) Just
#endif

sorobanToChunk :: Soroban -> [Chunk]
sorobanToChunk (Soroban _ ps)  = map makeCluster $ groupBy' isConnected ps where
    makeCluster xs = (head xs, length xs)
    isConnected p1  p2 = p1 + 1 == p2

chunkToSoroban :: Size -> [Chunk] -> Soroban
chunkToSoroban len cs = Soroban len ps where
    (_, ps) = g cs 0 []
    g [] n' ps' = (n', ps')
    g (x:xs) n' ps' = g xs (n'+ num) (ps'++ new) where
        num = snd x
        offset = fst x
        new = createBalls offset num
    createBalls _      0   = []
    createBalls offset num = offset : createBalls (offset + 1) (num - 1)

move :: Soroban -> Position -> Position -> Maybe Soroban
move (Soroban n ps) pOld pNew = let
        ps' = if
            | pOld < pNew -> moveR n (pOld, pNew) ps
            | pOld > pNew -> moveL n (pOld, pNew) ps
            | otherwise -> Nothing
        in
            Soroban n <$> ps'

stepR :: Size -> Position -> [Position]  -> Maybe [Position]
stepR n p ps = do
        pRightSpace <- listToMaybe $ [(p+1)..(n-1)] \\ ps
        let ps' = map (\x -> if x `elem` [p .. pRightSpace-1] then x+1 else x) ps
        toMaybe (p `elem` ps) ps'

moveR :: Size -> (Position, Position) -> [Position]  -> Maybe [Position]
moveR n (pOld, pNew) ps = foldM (flip $ stepR n) ps [pOld..(pNew-1)]

moveL :: Size -> (Position, Position) -> [Position]  -> Maybe [Position]
moveL n (pOld, pNew) ps = let
        reflectPos p = n-p-1
        reflect = reverse . map reflectPos
        pOld' = reflectPos pOld
        pNew' = reflectPos pNew
        ps' = reflect ps
        in
            reflect <$> moveR n (pOld', pNew') ps'

