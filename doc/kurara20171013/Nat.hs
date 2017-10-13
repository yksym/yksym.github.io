{-# Language StandaloneDeriving, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns, TypeSynonymInstances, FlexibleInstances #-}

import Fix

--type N = Either ()
type N = Maybe

orN :: (() -> c) -> (b -> c) -> N b -> c
orN f g m = maybe (f ()) g m

-- Haskellは general recursion 許してるからcoinductiveなデータ型もinductiveと同じキーワードでかけるので Fix は最小不動点かつ最大不動点を与える？？
-- CoNatは無限を含む必要がある(prevしても変化しない値がある)という記述があるが...fix SN はNatにも含まれるし・・・
type Nat = Fix N

pattern ZN :: Nat
pattern ZN = Fix Nothing

pattern SN :: Nat -> Nat
pattern SN n = Fix (Just n)


addN :: Nat -> Nat -> Nat
addN y = cata phi where
    phi = const y `orN` \n -> SN n


mulN :: Nat -> Nat -> Nat
mulN y = cata phi where
    phi = const ZN `orN` \n -> addN y n


predN :: Nat -> N Nat
predN = out

-- Lambek Theorem
predN' :: Nat -> N Nat
predN' = cata phi where
    phi = fmap (const ZN `orN` SN)


addN' :: Nat -> Nat -> Nat
addN' = curry $ ana phi where
    phi :: (Nat, Nat) -> N (Nat, Nat)
    phi ((predN -> Nothing),(predN -> Nothing)) = Nothing
    phi ((predN -> Just x), y) = Just (x, y)
    phi (x, (predN -> Just y)) = Just (x, y)


factN :: Nat -> Nat
factN = para phi where
    phi = const (SN ZN) `orN` \(r,x) -> mulN (SN x) r

infinity :: Nat
infinity = fix SN

instance Num Nat where
    negate ZN = ZN
    (+) = addN
    (*) = mulN
    fromInteger = ana phi where
        phi 0 = Nothing
        phi n = Just (n-1)
    abs = id
    signum = const $ SN ZN

instance Show Nat where
    show = show . (cata phi) where
        phi = const 0 `orN` \n -> n + 1

main = do
    print $ (==3) $ 1 `addN` 2
    print $ (==2) $ 0 `addN` 2
    print $ (==2) $ 2 `addN` 0
    print $ (==0) $ 0 `addN` 0

    print $ (==3) $ 1 `addN'` 2
    print $ (==2) $ 0 `addN'` 2
    print $ (==2) $ 2 `addN'` 0
    print $ (==0) $ 0 `addN'` 0

    print $ (==2) $ 1 `mulN` 2
    print $ (==0) $ 0 `mulN` 2
    print $ (==0) $ 2 `mulN` 0

    print $ (==Nothing) $ predN 0
    print $ (==Just 0) $ predN 1
    print $ (==Just 1) $ predN 2

    print $ (==Nothing) $ predN' 0
    print $ (==Just 0) $ predN' 1
    print $ (==Just 1) $ predN' 2

    print $ (==6) $ factN $ 3

