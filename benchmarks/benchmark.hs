import qualified Data.Conduit      as C
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Feeder       as F
import qualified Data.Predator       as Pd
import qualified Data.Machine      as M
import qualified Pipes             as P
import qualified Pipes.Prelude     as P
import Data.Functor.Identity
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

value :: Int
value = 1000

sourceM = M.enumerateFromTo 1 value
sourceC = C.enumFromTo 1 value
sourceP = P.each [1..value]

sourcePd :: Pd.Prey Int Identity ()
sourcePd = Pd.yieldMany [1..value]

sourceF :: F.Feeder Int Identity Identity ()
sourceF = F.yieldMany [1..value]

main = defaultMain
  [ bgroup "scan"
      [ bench "feeders" $ whnf drainF (F.scan (+) 0)
      , bench "predators" $ whnf drainPd (Pd.scan (+) 0)
      , bench "machines" $ whnf drainM (M.scan (+) 0)
      , bench "pipes" $ whnf drainP (P.scan (+) 0 id)
      , bench "conduit" $ whnf drainC (CC.scanl (+) 0)
      ]
  ]
