{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- User-friendlier syntax for writing the model
-- (constructing the node trace)

module Syntax where

import Control.Monad.State
import Control.Applicative
import qualified Data.IntSet as Set

import qualified Memory as Mem

import Metropolis
import Distribution (Dist(..))

-- reexporting and renaming
bern    = bernS
categorical :: Eq a => [(a,Double)] -> DistK KResampleable a
categorical = categoricalS
uniformly :: Eq a => [a] -> DistK KResampleable a
uniformly = uniformlyS
uniform   = uniformS
normal    = normalS
gamma     = gammaS
beta      = betaS

{-
  Main design criteria for the user-visible Syntax:
   -- we want to statically prevent conditioning where the observed values
      are the ones computed during the course of the program.
      The observed value must be the one known before the program starts.
   -- proper conditioning is done only on sampleable nodes, on random
      variables -- not on arbitrary boolean formulas
      So, we have to statically distinguish nodes (or, random variables),
      constructed from distributions, and expressions involving nodes.
      To avoid the Borel paradox.
      General conditioning could be programmed, e.g., with simulated
      annealing.
   -- The whole program should be a node (but we can convert an expression
   -- to a node if needed to be)

One may wish to write niceex1 as follows, in a nice functional syntax

niceex1 =
  let c = uniformN (Val 0) (Val 1)
      d = bernN (Val 0.5)
      e = bernN c
  in diracN (liftN2 (&&) e d)

but this will not work in the slightly more complex example

  let c = uniformN (Val 0) (Val 1)
      d = bernN (Val 0.5)
      e = betaN c c
      ...

which uses 'c' twice. Since c is a computation to build the node, the
naive implementation will duplicate 'c'. We need to detect
sharing. This is the same main problem that vexes us in representing
circuits in Haskell. So, the model construction has to be in a
monad -- at least so that we can generate unique names and detect
sharing.

-}


-- A model produces a stochastic expression, which is converted
-- to a node if needed.
-- That is, in the end, the whole program is represented by
-- the final node, which depends on the other nodes.
type Model a = MCMCM (SExp a)

-- Stochastic expression
data SExp a where
  Val  :: a -> SExp a          -- immediate (a Haskell) value, e.g., literal
  NRef :: NodeRef a -> SExp a  -- reference to a constructed node
  -- General computation over possibly several nodes
  NT   :: NTree   a -> (a -> b) -> SExp b

data NTree a where
  NTLeaf :: NodeRef a  -> NTree a
  NTPair :: NTree a -> NTree b -> NTree (a,b)

-- Perform a computation on a sigle SExp, doing a bit of
-- partial evaluation
-- That is why we represent the node computations as (NT t g),
-- with the separate tuple of nodes and the combining function.
instance Functor SExp where
  fmap f (Val x)  = Val (f x)
  fmap f (NRef x) = NT (NTLeaf x) f
  fmap f (NT t g) = NT t (f . g)

instance Applicative SExp where
  pure = Val

  Val  f <*> Val y  = Val (f y)
  -- NRef f <*> Val y  = Cannot happen: there are no distributions over
  -- functions
  Val f <*> NRef y  = NT (NTLeaf y) f
  -- (NRef x) (NRef y) = cannot happen
  NT t g <*> Val y  = NT t (\ab -> g ab y)
  Val f  <*> NT t g = NT t (\ab -> f (g ab))
  NT t g <*> NRef y = NT (NTPair t (NTLeaf y)) (uncurry g)
  -- NRef x <*> NT t g = cannot happen
  NT t1 g1 <*> NT t2 g2 = NT (NTPair t1 t2) (\ (ab,cd) -> g1 ab (g2 cd))

-- But SExp is NOT a Monad. Can you guess why?

instance Num a => Num (SExp a) where
  fromInteger = pure . fromInteger
  x + y = liftA2 (+) x y
  x - y = liftA2 (-) x y
  x * y = liftA2 (*) x y
  negate = fmap negate
  abs    = fmap abs
  signum = fmap signum

instance Fractional a => Fractional (SExp a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

pair :: SExp a -> SExp b -> SExp (a,b)
pair = liftA2 (,)

type family Args a :: *
type instance Args (DistK k a) = Model a
type instance Args (b -> a)   = SExp b -> Args a

class StochasticPrim a where
  dist :: a -> Args a

instance Show a => StochasticPrim (DistK k a) where
  dist d = fmap NRef $ with_node_none d

instance Show b => StochasticPrim (a -> DistK k b) where
  dist d x = fmap NRef $ with_node_ctor x d

instance Show c => StochasticPrim (a -> b -> DistK k c) where
  dist d x y = fmap NRef $ with_node_ctor (pair x y) (uncurry d)


{-
-- Node constructors for the end user. These ones are exported.
uniformN :: NodeCTor k1 Double -> NodeCTor k2 Double -> Model Double
uniformN x y = fmap NRef $
               with_node_ctor (pair x y) (uncurry uniform)
-}

-- Since Dirac is special and frequently used, we define the synonym for
--  dist dirac x
-- It turns a general node computation or a literal into a single node.
-- NB: if x is just a NoderRef, (diracN x) returns this reference (shares)
-- rather than creating a copy of it!
diracN :: Show a => SExp a -> Model a
diracN (Val x)  = fmap NRef $ create_node Nothing (diracS x)
diracN x@NRef{} = return x
diracN x = fmap NRef $ with_node_ctor x diracS




-- It is now statically ensured that we can condition only on
-- external data. We cannot condition on the results produced
-- during the computation.

-- condition is the transformation on DistK, which is lifted
-- functorially
class Condition a where
  type CondR a :: *
  type CondV a :: *
  condition :: CondV a -> a -> CondR a

instance Condition (DistK KResampleable a) where
  type CondR (DistK KResampleable a) = DistK KObserved a
  type CondV (DistK KResampleable a) = a
  condition = conditioned

instance Condition b => Condition (a -> b) where
  type CondR (a -> b) = a -> CondR b
  type CondV (a -> b) = CondV b
  condition v f = \x -> condition v (f x)


mcmC :: Show a => Integer -> Model a -> [a]
mcmC limit m = mcmc limit (m >>= to_node)

-- These are internal functions and should not be exported and used
-- by the end user
eval_nctor :: SExp a -> a
eval_nctor (Val x)  = x
eval_nctor (NRef x) = nref_val x
eval_nctor (NT t g) = g $ eval_ntree t

eval_ntree :: NTree a -> a
eval_ntree (NTLeaf x)     = nref_val x
eval_ntree (NTPair t1 t2) = (eval_ntree t1, eval_ntree t2)

-- If it is not a node, make a Dirac node
to_node :: Show a => SExp a -> MCMCM (NodeRef a)
to_node (Val x)  = create_node Nothing (diracS x)
to_node (NRef x) = return x
to_node x        = with_node_ctor x diracS

-- Generalization of with_node for the arbitrary tuple of dependent
-- nodes
with_node_ctor :: Show b => SExp a -> (a -> DistK k b) -> TraceM b
with_node_ctor (Val x) df  = create_node Nothing (df x)
with_node_ctor (NRef x) df = with_node x df
with_node_ctor (NT (NTLeaf x) g) df = with_node x (df . g)
with_node_ctor (NT (NTPair (NTLeaf x) (NTLeaf y)) g) df =
  with_node2 (x,y) (df . g)
with_node_ctor ndr@(NT t g) df = do
  let dist = df (eval_nctor ndr)
  nnew <- create_node (Just $ doupdate t (df . g)) dist
  register_dependencies t (Mem.addr nnew)
  return nnew
 where
   -- The update never resamples. The val may change because it depends
   -- on the node value, and LL may also change even if val stays the same.
   -- The self NodeRef is always the last argument.
 doupdate :: NTree b -> (b -> DistK k a) -> NodeRef a ->
             MCMCM (NodeRef a, NodeSet)
 doupdate ndrold df nsold = do
  ns       <- refresh_nref nsold            -- node being updated
  (tv,ts)  <- reeval_tree ndrold            -- node on which we depend
  let self = Mem.dref ns
  tnow <- fmap now get
  -- the parent node must have been updated
  when (ts <= tstamp self) $ fail (unwords ["dependency violation!",
      "node", show (Mem.addr ns), "depends on tree",
      "whose tstamp", show ts])
  let self' = case df tv of
        Dirac v -> self{val = v,tstamp=tnow}
        -- don't update the tstamp if only LL changes
        -- !!! Don't forget to update the resampler as well!
        Resampleable (Dist uns samp) ->
          self{ll = uns (val self), resampler = samp}
        Observed _ uns ->
          self{ll = uns (val self)}
  return (Mem.modify_ref self' ns,Set.empty)

 -- The node na depends on all nodes in NTree. Register these
 -- dependencies
 register_dependencies :: NTree a -> NodeAddress -> MCMCM ()
 register_dependencies (NTLeaf ndr) na = new_dependency ndr na
 register_dependencies (NTPair t1 t2) na = do
   register_dependencies t1 na
   register_dependencies t2 na

 -- re-evaluate the tree and return the value and the max timestamp
 reeval_tree :: NTree a -> MCMCM (a,TStamp)
 reeval_tree (NTLeaf ndrold) = do
   ndr <- refresh_nref ndrold
   let nd = Mem.dref ndr
       ts = tstamp nd
   return $ (val $ nd, ts)
 reeval_tree (NTPair t1 t2) = do
   (t1v,ts1) <- reeval_tree t1
   (t2v,ts2) <- reeval_tree t2
   return ((t1v,t2v),max ts1 ts2)

if_ :: Show a => SExp Bool -> Model a -> Model a -> Model a
if_ test th el = do
  -- Always make a new node, for the sake of ifnode
  entry <- with_node_ctor test diracS
  fmap NRef $ ifnode entry (th >>= to_node) (el >>= to_node)

-- Examples in the user-friendly syntax
niceex1 = do
  c <- dist uniform 0 1
  d <- dist bern 0.5
  e <- dist bern c
  diracN ((&&) <$> e <*> d)

niceex1r = mcmC 7 niceex1
-- All True

nice_prog_mult_conditions c1 c2 = do
  b <- dist beta 1 1
  dist (c1 `condition` bern) b
  dist (c2 `condition` bern) b
  diracN b

niceexmcr11 = sum $ mcmC 1000 (nice_prog_mult_conditions True False)
-- 499.799988743311

niceexmcr12 = sum $ mcmC 1000 (nice_prog_mult_conditions False False)
-- 257.2928205920028

prog_mixture1 cv = do
  c <- dist bern 0.5
  if_ c (dist (cv `condition` normal)  1 1)
        (dist (cv `condition` uniform) 0 3)
  return c

prog_mixture1_run = mcmC 20 (prog_mixture1 (-2))
-- all true



exif1 = do
  c <- dist bern 0.5
  d <- if_ c (dist normal ((fromIntegral . fromEnum) <$> c) 1)
             (diracN 5)
  e <- if_ (not <$> c) (dist normal 20 1) (diracN d)
  diracN (d + e)

exif1r = mcmC 7 exif1

exif2 = do
  c <- dist bern 0.5
  if_ c (cl1 10) (cl1 20)
 where
   cl1 x = do
     d <- dist bern 0.5
     if_ d cl3 (diracN x)
     
   cl3 = do
     a <- dist uniform 0 1
     b <- dist uniform 0 1
     diracN (a + b)
     

exif2r = mcmC 17 exif2

-- Programs from my messages about the problem with conditionals
tac1 = do
  c <- dist bern 0.5
  if_ c
    (dist normal 0 1)
    (dist uniform 10 20)
  return c

tac1_run = length . filter id $ mcmC 1000 tac1
-- 620 True

tac2 = do
  c <- dist bern 0.5
  if_ c
    (diracN (Val True) >> dist normal 0 1)
    (dist uniform 10 20)
  return c

tac2_run = length . filter id $ mcmC 1000 tac2
-- 523 True

tac3 = do
  c <- dist bern 0.5
  if_ c
    (diracN (Val True) >> dist normal 0 1)
    (dist uniform 10 20 >>= diracN)
  return c

tac3_run = length . filter id $ mcmC 1000 tac3
-- 532 True

-- tac2 and tac3 produce identical results, as expected:
-- since diracN is unit, e >>= diracN is indeed the same as diracN

tac10 = do
  c <- dist bern 0.5
  if_ c
    -- then
    (dist (0 `condition` normal) 0 1)
    -- else
    (dist (0 `condition` uniform) 10 20)
  return c

tac10_run = length . filter id $ mcmC 100 tac10
-- 100  -- all True

tac11 = do
  c <- dist bern 0.5
  if_ c
    -- then
    (dist (0 `condition` normal) 0 1)
    -- else
    (do
        d <- diracN 10
        dist (0 `condition` uniform) d 20)
  return c

tac11_run = length . filter id $ mcmC 100 tac11
-- 100  -- all True
