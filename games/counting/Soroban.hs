{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE CPP #-}
--{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Soroban
    ( Soroban(..)
    , Position
    , Size
    , Block
#ifndef __HASTE__
    , readSoroban
#endif
    , sorobanToBlock
    , blockToSoroban
    , move
    ) where

--import Debug.Trace
import Data.List
--import Data.Maybe
import Control.Arrow
#ifndef __HASTE__
import Text.Parsec
#endif
--import Control.DeepSeq
--import GHC.Generics (Generic)
import Util

debugShow :: b -> c -> c
--debugShow = traceShow
debugShow = flip const

type Position = Int
type Size     = Int
type Block = (Position, Size)

-- spec of pieces: idx1 < idx2 <=> pPos1 < pPos2
data Soroban = Soroban {
  numPiece :: Size,
  pieces :: [Position]
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


sorobanToBlock :: Soroban -> [Block]
sorobanToBlock (Soroban _ ps)  = map makeCluster $ groupBy' isConnected ps where
    makeCluster xs = (head xs, length xs)
    isConnected p1  p2 = p1 + 1 == p2

blockToSoroban :: Size -> [Block] -> Soroban
blockToSoroban len cs = Soroban len ps where
    (_, ps) = g cs 0 []
    g [] n' ps' = (n', ps')
    g (x:xs) n' ps' = g xs (n'+ num) (ps'++ new) where
        num = snd x
        offset = fst x
        new = createPieces offset num
    createPieces _      0   = []
    createPieces offset num = offset : createPieces (offset + 1) (num - 1)


move :: Soroban -> Position -> Position -> Maybe Soroban
move (Soroban n ps) pOld pNew = let
        ps' = if
            | pOld < pNew -> moveR n (pOld, pNew) ps
            | pOld > pNew -> moveL n (pOld, pNew) ps
            | otherwise -> Nothing
        in
            Soroban n <$> ps'

moveR :: Size -> (Position, Position) -> [Position]  -> Maybe [Position]
moveR n (pOld, pNew) ps = do
        idx <- elemIndex pOld ps
        let pNum = length ps
        let (l,r) = break (>= pOld) ps -- r contais pOld
        let rNum = pNum - idx
        let maxPos = n - rNum
        let pNew' = min pNew maxPos
        let (numMove, rest) = thrust pNew' r
        toMaybe (pNew' /= pOld) $ l ++ take numMove [pNew'..] ++ rest

-- any element in ps is ge pFrom.(to right)
thrust :: Position -> [Position]  -> (Size, [Position])
thrust pTo ps = let
        (mv, rest) = break (> pTo) ps
        numMove = length mv
        in if numMove == 0
            then (0, rest)
            else first (+ numMove) $ thrust (pTo + numMove) rest

reflectPos :: Size -> Position -> Position
reflectPos n p = n-p-1

reflect :: Size -> [Position] -> [Position]
reflect n = reverse . map (reflectPos n)

moveL :: Size -> (Position, Position) -> [Position]  -> Maybe [Position]
moveL n (pOld, pNew) ps = let
        pOld' = reflectPos n pOld
        pNew' = reflectPos n pNew
        ps' = reflect n ps
    in debugShow (ps, ps', pOld', pNew') $ reflect n <$> moveR n (pOld', pNew') ps'


