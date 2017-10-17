{-# LANGUAGE Rank2Types, BangPatterns, ScopedTypeVariables, GeneralizedNewtypeDeriving, GADTs, LambdaCase #-}
module Data.Predator where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Bifunctor
import Data.Functor.Sum
import Data.Type.Equality

-- | Monadic producer
data Prey s m a = Yield s (Prey s m a)
  | Lift (m (Prey s m a))
  | Pure a

instance MonadTrans (Prey s) where
  lift = Lift . fmap Pure

instance Monad m => Functor (Prey s m) where
  fmap = liftM

instance Monad m => Applicative (Prey s m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (Prey s m) where
  return = Pure
  Pure a >>= k = k a
  Yield s c >>= k = Yield s $ c >>= k
  Lift m >>= k = Lift $ fmap (>>=k) m
  Pure _ >> k = k
  Yield s c >> k = Yield s $ c >> k
  Lift m >> k = Lift $ fmap (>>k) m

killPrey :: Monad m => Prey s m a -> m a
killPrey = \case
  Pure a -> return a
  Yield _ k -> killPrey k
  Lift m -> m >>= killPrey

-- | Produce one element.
yield :: Monad m => s -> Prey s m ()
yield s = Yield s $ Pure ()
{-# INLINE yield #-}

-- | Produce many values.
yieldMany :: (Foldable f, Monad m) => f s -> Prey s m ()
yieldMany = foldr Yield (Pure ())
{-# INLINE yieldMany #-}

-- | Monadic consumer
newtype Predator s n m a = Predator { unPredator :: forall x r. Prey s n x
    -> (a -> Prey s n x -> m r) -> m r -> m r }

instance Functor (Predator s n m) where
  fmap = liftM

instance Applicative (Predator s n m) where
  pure = return
  (<*>) = ap
  (*>) = (>>)

instance Monad (Predator s n m) where
  return a = Predator $ \s k _ -> k a s
  Predator m >>= k = Predator $ \s cont e -> m s (\a s' -> unPredator (k a) s' cont e) e
  Predator m >> Predator n = Predator $ \s cont e -> m s (\_ s' -> n s' cont e) e

instance MonadTrans (Predator s n) where
  lift m = Predator $ \s k _ -> m >>= \a -> k a s

instance Alternative (Predator s n m) where
  empty = Predator $ \_ _ e -> e
  Predator m <|> Predator n = Predator $ \s cont e -> m s cont (n s cont e)

awaitOn :: forall n m s. (Monad n, Monad m) => (forall x. n x -> m x) -> Predator s n m s
awaitOn t = Predator go where
  go :: Prey s n x -> (s -> Prey s n x -> m r) -> m r -> m r
  go prey cont end = case prey of
    Pure _ -> end
    Yield s k -> cont s k
    Lift m -> t m >>= \r -> go r cont end
{-# INLINE awaitOn #-}

await :: Monad m => Predator s m m s
await = awaitOn id
{-# INLINE await #-}

sinkNull :: Monad m => Predator s m m ()
sinkNull = forever await

sinkList :: Monad m => Predator s m m [s]
sinkList = optional await >>= \case
  Nothing -> return []
  Just x -> (x:) <$> sinkList

type Heterotroph a b m = Predator a m (Prey b m)

prey :: Monad m => Predator s n m a -> Prey s n x -> m (Maybe (a, Prey s n x))
prey m s = unPredator m s (\a r -> return $ Just (a, r)) $ return Nothing

scan :: Monad m => (b -> a -> b) -> b -> Heterotroph a b m ()
scan f b = do
  a <- awaitOn lift
  let !b' = f b a
  lift $ yield b'
  scan f b'

(@->) :: Monad m => Prey a m x -> Heterotroph a b m r -> Prey b m (Maybe r)
p @-> h = prey h p >>= \case
    Nothing -> return Nothing
    Just (a, p') -> do
      lift $ killPrey p'
      return (Just a)

(>->) :: Monad m => Heterotroph a b m r -> Heterotroph b c m r -> Heterotroph a c m r
Predator p >-> Predator h = Predator $ \pa cont end -> h (p pa (\r pa' -> lift $ killPrey pa') (lift $ killPrey pa)) (\r pa' -> cont r (lift $ killPrey pa')) end
