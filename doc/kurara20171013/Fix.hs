{-# Language StandaloneDeriving, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Fix (
    Fix(..)
  , out
  , cata
  , ana
  , para
  , apo
  , fix
) where

import Data.Function (on,fix)
import Data.Functor
import Control.Arrow ((&&&), (|||))

-----------------
-- F代数
-----------------

-- [f,g] = f ||| g
-- <f.g> = f &&& g
-- (g &&& h) . f = (g.f) &&& (h.f)

newtype Fix f = Fix (f (Fix f))

instance Eq (f (Fix f)) => Eq (Fix f) where
    (==) = (==) `on` out

out :: Fix f -> f (Fix f)
out (Fix x) = x

cata :: Functor f => (f c -> c) -> Fix f -> c
cata phi = phi . fmap (cata phi) . out

ana :: Functor f => (c -> f c) -> c -> Fix f
ana phi = Fix . fmap (ana phi) . phi

para :: Functor f => (f (c, Fix f) -> c) -> Fix f -> c
para phi = fst . cata (phi &&& (Fix . fmap snd))

apo :: Functor f => (c -> f (Either c (Fix f))) -> c -> Fix f
apo phi = ana (phi ||| fmap Right . out) . Left


