{-# Language StandaloneDeriving, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

import Fix

data L a x = N | C a x deriving (Show, Eq)

orL :: (() -> c) -> ((a,b) -> c) -> L a b -> c
orL f g N = f ()
orL f g (C a x) = g (a,x)

instance Functor (L a) where
    fmap f N = N
    fmap f (C x xs) = C x (f xs)

-- この定義だとFunctor諸々がderiving出来ない??
type List a = Fix (L a)

pattern NL :: List a
pattern NL = Fix N

pattern CL :: a -> List a -> List a
pattern CL x xs = Fix (C x xs)

lengthL :: List a -> Int
lengthL = cata phi where
    phi = const 0 `orL` \(x,n) -> 1 + n

appendL :: List a -> List a -> List a
appendL xs ys = cata phi xs where
    phi = const ys `orL` \(x,xs) -> CL x xs

mapL :: (a -> b) -> List a -> List b
mapL f = cata phi where
    phi = const NL `orL` \(a,bs) -> CL (f a) bs

filterL :: (a -> Bool) -> List a -> List a
filterL p = cata phi where
    phi = const NL `orL` \(x,xs) -> if p x then CL x xs else xs

dropWhileL :: (a -> Bool) -> List a -> List a
dropWhileL p = para phi where
    phi = const NL `orL` \(x,(r,xs)) -> if p x then r else CL x xs

-- 最初に y < x になった部分で挿入
insertL :: (Ord a) => a -> List a -> List a
insertL y = para phi where
    phi = const NL `orL` \(x,(r,xs)) -> if y < x then CL y (CL x xs) else CL x r -- r は後段に対しても再帰的にをinsertした結果、 xs は何もしてない

toList :: List a -> [a]
toList = cata phi where
    phi = const [] `orL` \(x,xs) -> (x:xs)

fromList :: [a] -> List a
fromList = ana phi where
    phi []     = N
    phi (x:xs) = C x xs


main = do
    print $ (==[]) $ toList $ fromList ([] :: [Int])
    print $ (==[1,2]) $ toList $ fromList [1,2]
    print $ (== 2) $ lengthL $ fromList [1,2]
    print $ (== fromList [1,2,3]) $ appendL (fromList [1,2]) (fromList [3])
    print $ (== fromList [1,2]) $ filterL (<3) (fromList [1,3,2,3])
    print $ (== fromList [3]) $ dropWhileL (<3) (fromList [1,2,3])
    print $ toList $ insertL 2 (fromList [1,3,5])

