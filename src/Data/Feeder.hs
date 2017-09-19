{-# LANGUAGE Rank2Types, BangPatterns, ScopedTypeVariables, GeneralizedNewtypeDeriving, GADTs, LambdaCase #-}
module Data.Feeder where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Bifunctor
import Data.Functor.Sum
import Data.Type.Equality

-- | Monadic consumer
data Eater s m a = Await (Maybe s -> Eater s m a)
    | Leftover s (Eater s m a)
    | Lift (m (Eater s m a))
    | Pure a

instance MonadTrans (Eater s) where
  lift = Lift . fmap Pure

instance Monad m => Functor (Eater s m) where
  fmap = liftM

instance Monad m => Applicative (Eater s m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (Eater s m) where
  return = Pure
  Pure a >>= k = k a
  Await f >>= k = Await ((>>= k) . f)
  Lift m >>= k = Lift $ fmap (>>=k) m
  Leftover s c >>= k = Leftover s (c >>= k)

killEater :: Monad m => Eater s m a -> m a
killEater = \case
  Pure a -> return a
  Await k -> killEater $ k Nothing
  Leftover _ k -> killEater k
  Lift m -> m >>= killEater

-- | Monadic producer
newtype Feeder s n m a = Feeder { unFeeder :: forall x r. Eater s n x -> (a -> Eater s n x -> m r) -> m r }

instance Functor (Feeder s n m) where
  fmap = liftM

instance Applicative (Feeder s n m) where
  pure = return
  (<*>) = ap
  (*>) = (>>)

instance Monad (Feeder s n m) where
  return a = Feeder $ \s k -> k a s
  Feeder m >>= k = Feeder $ \s cont -> m s $ \a s' -> unFeeder (k a) s' cont
  Feeder m >> Feeder n = Feeder $ \s cont -> m s $ \_ s' -> n s' cont

instance MonadTrans (Feeder s n) where
  lift m = Feeder $ \s k -> m >>= \a -> k a s

yieldOn :: Monad m => (forall x. n x -> m x) -> s -> Feeder s n m ()
yieldOn t s = Feeder $ \e cont -> case e of
  Pure a -> cont () $ Leftover s (Pure a)
  Await k -> cont () $ k (Just s)
  Leftover s c -> unFeeder (yieldOn t s) c cont
  Lift m -> t m >>= \c -> unFeeder (yieldOn t s) c cont

yield :: Monad m => s -> Feeder s m m ()
yield = yieldOn id
{-# INLINE yieldOn #-}

await :: Eater s m (Maybe s)
await = Await Pure
{-# INLINE await #-}

sinkNull :: Monad m => Eater s m ()
sinkNull = await >>= maybe (return ()) (const sinkNull)

sinkList :: Monad m => Eater s m [s]
sinkList = await >>= \case
  Nothing -> return []
  Just x -> (x:) <$> sinkList

type Rancher a b m = Feeder b m (Eater a m)

feed :: Monad m => Feeder s n m a -> Eater s n x -> m (a, Eater s n x)
feed m s = unFeeder m s ((return.) . (,))

scan :: Monad m => (b -> a -> b) -> b -> Rancher a b m ()
scan f b = lift await >>= \case
  Nothing -> return ()
  Just a -> do
    let !b' = f b a
    yieldOn lift b'
    scan f b'

(>-$) :: Monad m => Rancher a b m x -> Eater b m r -> Eater a m r
r >-$ e = do
  (a, e') <- feed r e
  lift $ killEater e'
