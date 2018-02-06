module Core.Optimise.Propagate
  ( trivialPropag
  , constrPropag
  ) where


import qualified Data.Map.Strict as Map
import Data.Traversable
import Data.Function
import Data.Either
import Data.Triple
import Data.List

import Syntax (Var, Resolved)
import Core.Optimise

trivialPropag :: TransformPass
trivialPropag = pass' go where
  go (CotLet vs e) =
    let keep e@(v, _, t) = if trivial t then Left (Map.singleton v t)
                                        else Right e
        (ss, vs') = partitionEithers (map keep vs)

        trivial CotLit{} = True
        trivial (CotExtend e rs) = trivial e && all (trivial . thd3) rs
        trivial _ = False
     in CotLet vs' (substitute (mconcat ss) e)
  go x = x

constrPropag :: TransformPass
constrPropag = pass go where
  go (CotLet vs e) = do
    (keep, subst) <- partitionEithers <$> for vs (\v -> do
      cl <- conLike (thd3 v)
      if cl
         then do
           (new, bind) <- splitCon (thd3 v) <$> fresh
           pure (Right (Map.singleton (fst3 v) new, bind))
         else pure (Left v))
    let (ss, ks) = unzip subst
        eqf3 = (==) `on` fst3
    pure $ CotLet (unionBy eqf3 keep ks) (substitute (mconcat ss) e)
  go x = pure x

  conLike :: CoTerm -> Trans Bool
  conLike x = case stripTyApp x of
    CotApp (CotRef x _) _ -> isCon x
    _ -> pure False

  splitCon :: CoTerm -> Var Resolved -> (CoTerm, (Var Resolved, CoType, CoTerm))
  splitCon (CotApp con ca) v
    | tp <- findConstrTy con
    , CotyArr arg _ <- tp
    = (CotApp con (CotRef v arg), (v, arg, ca))
    where
      findConstrTy (CotTyApp v _) = findConstrTy v
      findConstrTy (CotRef _ tp) = tp
      findConstrTy t = error $ "splitCon unwind " ++ show t

  splitCon t _ = error $ "splitCon " ++ show t
