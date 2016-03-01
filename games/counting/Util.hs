{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
module Util
    ( whenJust
#ifdef __HASTE__
    , when
#endif
    , extract'
    , noop
    , groupBy'
    , toMaybe
    ) where
import Control.Monad.Trans.Reader

#ifdef __HASTE__
when      :: (Applicative f) => Bool -> f () -> f ()
when p s  = if p then s else pure ()
#endif

whenJust :: (Monad m) => Maybe (m a) -> m ()
whenJust = sequence_

extract' :: (Monad m) => ReaderT r m a -> ReaderT r m (m a)
extract' m = runReaderT m <$> ask

noop :: (Monad m) => m ()
noop = return ()

-- groupBy' : eq is applied with previous element.(In groupBy, applied with the first of the group.)
-- >>> groupBy' (\x y -> x + 1 == y) [0,2,3,5,6,7,8,10,13]
-- [[0],[2,3],[5,6,7,8],[10],[13]]
groupBy'            :: (Show a) => (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _  []      =  []
groupBy' eq (x:xs)  =  ys : groupBy' eq zs where
                                     (ys,zs) = go x xs
                                     go b0 []       = ([b0], [])
                                     go b0 (b1:bs) = if eq b0 b1
                                        then let (bs',cs') = go b1 bs in (b0:bs', cs')
                                        else ([b0], b1:bs)

toMaybe :: Bool -> a -> Maybe a
toMaybe True x = Just x
toMaybe False _ = Nothing


