{-# LANGUAGE BangPatterns, LambdaCase #-}
import qualified Data.Conduit      as C
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Feeder       as F
import qualified Data.Predator       as Pd
import qualified Data.Machine      as M
import qualified Data.Iteratee.Iteratee as I
import qualified Pipes             as P
import qualified Pipes.Prelude     as P
import qualified Data.Boombox as B
import qualified Data.Boombox.Tap as B
import qualified Streaming.Prelude as S
import qualified Tubes as T
import qualified Control.Arrow.Machine as Mc
import Data.Functor.Identity
import Control.Arrow
import Control.Monad
import Criterion.Main
import Data.List

drainM :: M.ProcessT Identity Int o -> ()
drainM m = runIdentity $ M.runT_ (sourceM M.~> m)

drainP :: P.Proxy () Int () a Identity () -> ()
drainP p = runIdentity $ P.runEffect $ P.for (sourceP P.>-> p) P.discard

drainC :: C.Conduit Int Identity a -> ()
drainC c = runIdentity $ (sourceC C.$= c) C.$$ C.sinkNull

drainPd :: Pd.Heterotroph Int a Identity () -> ()
drainPd h = fst $ runIdentity $ Pd.prey Pd.sinkNull $ sourcePd Pd.@-> h

drainF :: F.Rancher Int a Identity () -> ()
drainF h = runIdentity $ F.killEater $ snd $ runIdentity $ F.feed sourceF $ h F.>-$ F.sinkNull

drainI :: I.Nullable a => I.Enumeratee Int a Identity () -> ()
drainI h = runIdentity $ I.run $ runIdentity $ I.run $ runIdentity $ sourceI $ h $ I.mapChunksM_ $ const $ return ()

drainB :: B.Recorder Identity Identity Maybe Int a -> ()
drainB h = maybe () (\(_,_,r) -> r) $ sourceB B.@.$ h B.>-$ forever B.await

drainS :: (S.Stream (S.Of Int) Identity () -> S.Stream (S.Of a) Identity ()) -> ()
drainS h = runIdentity $ S.effects $ h sourceS

drainT :: T.Tube Int a Identity () -> ()
drainT h = runIdentity $ T.runTube $ sourceT T.>< h T.>< T.stop

drainMc :: Mc.ProcessA (->) (Mc.Event Int) a -> ()
drainMc h = Mc.run_ (h >>> arr (const Mc.noEvent)) [1..value]

instance I.NullPoint Int where
  empty = 0

instance I.Nullable Int where
  nullC = (==0)

value :: Int
value = 10000

sourceM = M.enumerateFromTo 1 value
sourceC = C.enumFromTo 1 value
sourceP = P.each [1..value]

sourcePd :: Pd.Prey Int Identity ()
sourcePd = Pd.yieldMany [1..value]

sourceF :: F.Feeder Int Identity Identity ()
sourceF = F.yieldMany [1..value]

sourceI :: I.Enumerator Int Identity a
sourceI = I.enumList [1..value]

sourceB :: B.Tape Identity Maybe Int
sourceB = B.tap [1..value]

sourceS :: Monad m => S.Stream (S.Of Int) m ()
sourceS = S.each [1..value]

sourceT :: Monad m => T.Tube () Int m ()
sourceT = T.each [1..value]

scanB :: (b -> a -> b) -> b -> B.Recorder Identity Identity m a b
scanB f = go where
  go b = B.Tape $ B.await >>= \x -> let !b' = f b x in return (b', pure $ go b')

scanT :: Monad m => (b -> a -> b) -> b -> T.Tube a b m x
scanT f = go where
  go b = T.await >>= \x -> let !b' = f b x in T.yield b' >> go b'

main = defaultMain
  [ bgroup "scan"
      [ bench "boombox" $ whnf drainB (scanB (+) 0)
      , bench "machinecell" $ whnf drainMc (Mc.evMap (+) >>> Mc.accum 0)
      , bench "streaming" $ whnf drainS (S.scan (+) 0 id)
      , bench "tubes" $ whnf drainT (scanT (+) 0)
      , bench "feeders" $ whnf drainF (F.scan (+) 0)
      , bench "predators" $ whnf drainPd (Pd.scan (+) 0)
      , bench "iteratee" $ whnf drainI (I.unfoldConvStream (\x -> I.liftI $ \case
        I.Chunk i -> let !r = x + i in return (r, r)
        I.EOF _ -> return (x, x)) 0)
      , bench "machines" $ whnf drainM (M.scan (+) 0)
      , bench "pipes" $ whnf drainP (P.scan (+) 0 id)
      , bench "conduit" $ whnf drainC (CC.scanl (+) 0)
      ]
  ]
