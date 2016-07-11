{-# LANGUAGE RankNTypes, NoMonomorphismRestriction#-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}

module Metropolis where

import System.Random (RandomGen, Random, StdGen, randomR, getStdGen,mkStdGen)

import Control.Monad.State
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as Set

import qualified Memory as Mem

import Distribution (Dist(..),LogLikelihood,
                     bern,categorical,dirac,uniformly,
                     uniform,normal,gamma,beta)
import qualified Debug.Trace as TR

-- Enable or disable debugging
-- debug str e = TR.trace str e
-- debugM str = debug_trace str
debug str e = e
debugM str = return ()

type LL = LogLikelihood


-- For the purposes of MCMC, we need to annotate Dist with their kind
data DistKind = KResampleable | KDirac | KObserved

data DistK :: DistKind -> * -> * where
  -- We can sample from this distribution,
  -- and we can resample (change our choice)
  Resampleable :: Dist a -> DistK KResampleable a
  -- Its value is fixed and its LogLikelihood is always 0
  -- Please see the note on dirac at the end of the file for the
  -- explanation why we have to distinguish dirac from other distributions.
  Dirac :: a -> DistK KDirac a
  -- In principle, we can resample from the distribution.
  -- However, its sample has been observed and cannot change.
  -- Since the parameters of the distribution may depend on previous choices
  -- that may have changed, LogLikelihood may need to be re-computed.
  Observed :: a -> (a -> LL) -> DistK KObserved a

instance Show a => Show (DistK k a) where
  show (Resampleable _) = "Resampleable"
  show (Dirac x)        = "Dirac: " ++ show x
  show (Observed x _)   = "Observed: " ++ show x

  -- Re-sample-able nodes are also recorded in the global
  -- state.

diracS :: a -> DistK KDirac a
diracS = Dirac

bernS :: Double -> DistK KResampleable Bool
bernS d = Resampleable (bern d)

uniformlyS :: Eq a => [a] -> DistK KResampleable a
uniformlyS lst = Resampleable $ uniformly lst

categoricalS :: Eq a => [(a,Double)] -> DistK KResampleable a
categoricalS lst = Resampleable (categorical lst)

uniformS :: Double -> Double -> DistK KResampleable Double
uniformS h l = Resampleable $ uniform h l

normalS :: Double -> Double -> DistK KResampleable Double
normalS x s = Resampleable $ normal x s

gammaS :: Double -> Double -> DistK KResampleable Double
gammaS a b = Resampleable $ gamma a b

betaS :: Double -> Double -> DistK KResampleable Double
betaS a b = Resampleable $ beta a b

sample :: DistK k a -> StdGen -> (a, LL, StdGen)
sample (Resampleable (Dist _ samplef)) g = samplef g
sample (Dirac x) g = (x,0,g)
sample (Observed x uns) g = (x,uns x,g)

distinfo :: DistK k a -> (DistKind, StdGen -> (a,LL,StdGen))
distinfo (Resampleable Dist{distSample=f}) = (KResampleable,f)
distinfo Dirac{}    = (KDirac, \_ -> error "Can't reseample Dirac")
distinfo Observed{} = (KObserved, \_ -> error "Can't reseample Observed")


-- ------------------------------------------------------------------------
-- Trace, the node DAG, the representation of the program execution

-- The algorithm re-executes the program many times.
-- TStamp counts the number of re-executions (that is, steps in
-- the Markov chain).
-- For each program point (see Node below) we identify
-- the time the node's value is computed

type TStamp = Int

-- A node in a trace
data Node a = Node{
  val       :: a,                        -- computed value
  ll        :: !LL,                      -- its LogLikelihood
  ident     :: a -> String,              -- Identification, for debugging
  resampler :: StdGen -> (a,LL,StdGen),  -- (re)sampling procedure
  kind      :: DistKind,                 -- if it resampleable. Non-resampleable
                                         -- do not count for cardinality
  -- The timestamp changes whenever the val of the sample changes.
  -- Changes in LL do not advance the timestamp.
  tstamp    :: !TStamp,

  -- If Just f, the node's LL depends on previous nodes
  -- and so may have to be re-computed.
  -- The difference from re-sample: for a node corresponding to a
  -- primitive distribution such as bern, the update procedure
  -- never changes the sample, the val of node. However, the distribution
  -- parameters may depend on values of other nodes. If they change,
  -- LL also changes and has to be updated.
  -- Update returns the updated node plus the set of nodes that
  -- have to be updated, in addition to the regular dependencies
  -- The extra list is used for ``control dependencies'' (nodes
  -- that became active, for example)
  update   :: Maybe (NodeRef a -> MCMCM (NodeRef a, NodeSet))
  }

instance Show (Node a) where
  show n = "Node{" ++
    ident n (val n) ++ ";  LL: " ++ show (ll n) ++ 
    "\nTstamp: " ++ show (tstamp n) ++ "}\n"

resampleable :: Node a -> Bool
resampleable Node{kind=KResampleable} = True
resampleable _ = False

-- The DAG of nodes
-- Nodes are stored in the global state of the sampler

type NodeAddress = Mem.Address
type NodeSet = Set.IntSet  -- set of node addresses
type NodeRef a = Mem.Ref (Node a)

nref_val :: NodeRef a -> a
nref_val = val . Mem.dref

-- MCMC state
data MCState = MCState {
  seed     :: StdGen,
  now      :: !TStamp,
  nodes    :: Mem.Memory Node,
  
  -- nodes that depend on the given one
  -- (and which should be updated if this one changes)
  deps     :: M.IntMap NodeSet,   

  -- Inactive nodes (nodes in inactive conditional branches)
  -- It simplifies the code for nested conditionals if we can deactivate
  -- already inactive nodes. Therefore, we maintain the deactivation
  -- count with each inactive node. The node becomes active (and has to
  -- be removed from the inactive map) when the count goes to 0.
  inactive :: M.IntMap Int,
  
  -- Nodes that should be updated but cannot be just yet, because
  -- they are inactive. When activated, they need to be updated.
  delayed_updates :: !NodeSet,
  
  -- LogLikelihood of all nodes. The node update (or add) procedures
  -- maintain this field
  llcum    :: !LL,
  
  -- list of nodes whose sample may be changed
  sampleable :: [NodeAddress],
  -- Record the added nodes (needed for compiling branches)
  added      :: NodeSet,
  -- The number of activated nodes minus the number of deactivated nodes
  card_change :: !Int,
  -- debug messages (useful for debugging)
  debug_msg  :: [String]
  }

-- The monad for MCMC computation
type MCMCM a = State MCState a
-- The computation of buiding the trace DAG
type TraceM a = MCMCM (NodeRef a)

-- a version of randomR in the MCMCM monad
randomS :: (Show a, Random a) => (a, a) -> MCMCM a
randomS xy = do
  s <- get
  let (r, g') = randomR xy (seed s)
  put s{seed=g'}
  return r

-- when :: Bool -> m () -> m ()
-- when True m = m
-- when _      = return ()

debug_trace :: [String] -> MCMCM ()
debug_trace strs = modify $ \s -> s{debug_msg = unwords strs : debug_msg s}

debugState :: String -> MCMCM ()
debugState str = do
  s <- get
  debugM [str,"\nState",
   "now",show (now s),
   ", deps", show (deps s),
   ", inactive", show (inactive s),
   ", delayed", show (delayed_updates s),
   ", sampleable", show (sampleable s),
   ", added", show (added s),
   ", card change", show (card_change s)]


-- Operations with nodes and node database
new_nref :: Node a -> TraceM a
new_nref n = do
  s <- get
  let (nr,mem') = Mem.new n (nodes s)
      na        = Mem.addr nr
      -- record updateable nodes
      sa = if resampleable n then na : sampleable s else sampleable s
  put s{nodes=mem', sampleable=sa, added=Set.insert na (added s),
        llcum = llcum s + ll n}
  -- debugM ["new nref:",show (Mem.addr nr)]
  return nr

-- Add a new dependency to the given NodeRef
new_dependency :: NodeRef a -> NodeAddress -> MCMCM ()
new_dependency nr na =
  modify $ \s ->
    s{deps = M.insertWith Set.union (Mem.addr nr)
                          (Set.singleton na) (deps s)}

-- Refresh the given NodeRef from memory
refresh_nref :: NodeRef a -> TraceM a
refresh_nref nr = do
  mem <- fmap nodes get
  return $ Mem.refresh nr mem

-- Store the modified nr in the Node memory, update llcum.
-- Return the list of the nodes that depend on nr.
-- We return the list if the modified node nr has its value changed,
-- that is, has a newer timestamp.
modify_node :: NodeRef a -> MCMCM NodeSet
modify_node nr = do
  s <- get
  let (nrold,nodes') = Mem.store nr (nodes s)
      nold = Mem.dref nrold
      n    = Mem.dref nr
  put s{nodes=nodes', llcum = llcum s + ll n - ll nold}
  return $ 
    if tstamp nold == tstamp n then Set.empty
    else M.findWithDefault Set.empty (Mem.addr nr) (deps s)

-- The queue of the nodes to update
-- NOTE: the ordering property: the elements (node identifiers)
-- are ordered such that a node with a smaller identifier does not
-- depend on the nodes with larger identifiers. If the node identifier
-- is the running counter that counts node creation, the invariant
-- is satisfied automatically. Since the language to describe the model,
-- the node graph, has no mutation or recursion, a node simply cannot
-- depend on nodes that are created later.
-- So, we chose NodeAddress as such node identifier, which advances
-- as nodes are created.
-- Another property of Set we explore is the uniqueness: a node may depend
-- on several nodes, and so could be flagged for updates multiple times,
-- if all its parent nodes are updated. We really need to update the node
-- only once -- after all its parent nodes are updated.
-- See the more detailed note on dependencies at the end of this code section.
type UpdateQueue = NodeSet

-- Propagate the update in one node through its dependencies
-- We chose the minimal element, for the reasons explained in the
-- note that follows the code.
propagate_update :: UpdateQueue -> MCMCM ()
propagate_update q | Set.null q = return ()
propagate_update q | (na,tail) <- Set.deleteFindMin q = do
  debugM ["\nupdating node",show na]
  s <- get
  if M.member na (inactive s)
    then do
    -- If the node is inactive, its update is delayed
    put s{delayed_updates=Set.insert na (delayed_updates s)}
    debugM ["delaying",show na]
    propagate_update tail
    else
    Mem.locate na (nodes s) $ \nr -> do
      let n = Mem.dref nr
      when (tstamp n >= now s) $
        fail "dependency violation: updating the already updated"
      -- invoke the update procedure, if any
      case update n of
        Nothing  -> propagate_update tail
        Just upd -> do
          (nr',extra_deps)   <- upd nr
          regular_dependents <- modify_node nr'
          let dependents = Set.union extra_deps regular_dependents
          debugM ["  dependents:",show dependents]
          propagate_update $ Set.union tail dependents

{-
A note on update propagation

The subtle issue is node depending on several nodes. How to make
sure the node is updated only once, and only after all of its dependents
are updated.

Consider the program (in the nice surface syntax, see Syntax.hs )

upp1 = do
  a <- uniformN (Val 0) (Val 1)
  b <- diracN (liftN2 (+) a (Val 2))
  c <- diracN (liftN2 (+) a (Val 3))
  d <- uniformN (liftN2 (+) a b) c
  return d
-}

upp1 =
  with_node_none (uniformS 0 1)        `next` (\a ->
  with_node a (\av -> diracS $ av + 2) `next` (\b ->
  with_node a (\av -> diracS $ av + 3) `next` (\c ->
  with_node2 (a,b) (\ (a,b) -> diracS $ a + b) `next` (\ab ->
  with_node2 (ab,c) (\ (ab,c) -> uniformS ab c)))))

{-
Here, d depends on all previous nodes. Suppose a is resampled, which
affects all other nodes.  When a, b and c are updated,
they put the update of d on the update queue. It is important
that d were updated only once, and only after _all_ of its
dependents have been updated. So, the update on d should be performed
only after b and c have been updated (rather than right after a has been
updated).

The dependency list for 'a' should look like [b,c,d].
So, when a is resampled, UpdateQueue
will look like [b,c,d]. After b is updated, it will look like
[c,d]. Finally, after c is updated, the dependency queue will look
like [d]. So, everything comes out right: d is updated only after
b and c are. 
-}

-- upp1r = mcmC 7 upp1
upp1r = mcmc 7 upp1

{-
To formalize: the desired property is that a node is updated only after
all of its dependents (that should be modified) are updated. Let's represent
dependencies as a graph (DAG): there is a directed edge from node a
to node b if after an update to 'a' node 'b' has to be updated. The update
propagation amounts to a traversal of the graph. The traversal should
have the property that when a node is visited, all of its parents must
have been visited as well.

The property is satisfied if, upon visiting the node we add all its children
in a Set. Next visited node will be the _smallest_ node in that Set,
smallest in the sense it was created before other nodes. So, the invariant
of the traversal is that a visited node has the creation time earlier than
any other node in the update queue. Since we do not mutate dependencies
after creation, a node cannot depend on nodes created later. Therefore,
when a node is picked from the Set to be visited next, none of its parents
can be in that Set.

Conditional branches bring a twist: a node may become active whose timestamp
is less than now. It may depend on the now that has been updated a while ago.
Therefore, when we check for the dependency violation, we use the node's
tstamp rather than tnow.
-}

-- Here is a trickier example.
{-
upp2 = do
  a <- uniformN (Val 0) (Val 1)
  b <- diracN (liftN2 (+) a (Val 2))
  c <- diracN (liftN2 (+) b (Val 3))
  d <- uniformN (liftN2 (+) a b) c
  return d

upp2r = mcmC 7 upp2
-}

-- ------------------------------------------------------------------------
-- Operations of the probabilistic language: building the trace
-- These operations are not meant for the end user: we offer
-- a more convenient syntax later.

create_node :: Show a =>
               Maybe (NodeRef a -> MCMCM (NodeRef a,NodeSet)) ->
               DistK k a -> TraceM a
create_node update dist = do
  g <- fmap seed get
  let (v,ll,g') = sample dist g
  modify $ \s -> s{seed=g'}
  t <- fmap now get
  let (kind,resampler) = distinfo dist
  new_nref Node{
    ident = show,
    kind = kind,
    val = v, ll=ll, resampler = resampler,
    tstamp = t,
    update = update
  }

-- A node that does not depend on any other nodes
with_node_none :: Show a => DistK k a -> TraceM a
with_node_none dist = create_node Nothing dist

-- One may object to the following type as being dangerous:
-- nothing stops the user from writing
--  with_node_none (uniformS 0 1) `next` (\n ->
--    with_node n (\nv -> condition nv (uniformS 0 1)))
-- that is, to condition on the value produced by the model
-- rather than on some external (to the model) observation.
-- Recall however that the end user will not see conditioned below.
-- The end user will use the surface syntax, described later.

conditioned :: a -> DistK KResampleable a -> DistK KObserved a
conditioned obs (Resampleable (Dist uns samp)) =
  Observed obs uns

-- We can make TraceM to be a monad by defining (>>=)
-- However, it is too `dynamic': the second argument of bind,
-- the function (a -> TraceM b) may produce arbitrary number of nodes.
-- It is difficult to keep track of these nodes and optimize.
-- Therefore, we define restricted versions of bind, such
-- as with_node, etc (to be combined with next). The operation
-- like with_node, by design, can only build a single node.


next :: TraceM a -> (NodeRef a -> TraceM b) -> TraceM b
next = (>>=)

-- Create a new node which depends on an existing one
-- (passed as the first argument, ndr). 
with_node :: Show b => NodeRef a -> (a -> DistK k b) -> TraceM b
with_node ndr f = do
  let dist = f (nref_val ndr)
  nnew <- create_node (Just $ doupdate ndr) dist
  new_dependency ndr (Mem.addr nnew)
  return nnew
 where
   -- The update never resamples. The val may change because it depends
   -- on the node value, and LL may also change even if val stays the same.
   -- The self NodeRef is always the last argument.
   -- See the note on dirac at the end of the file, explaining why
   -- updates never resamples yet dirac is treated specially.
 doupdate ndrold nsold = do
  ns   <- refresh_nref nsold                -- node being updated
  ndr  <- refresh_nref ndrold               -- node on which we depend
  let nd   = Mem.dref ndr
      self = Mem.dref ns
  tnow  <- fmap now get
  inact <- fmap inactive get 
  when (M.member (Mem.addr ndr) inact || M.member (Mem.addr ns) inact) $
    fail (unwords
          ["the parent", show (Mem.addr ndr), "of", show (Mem.addr ns),
           "or the node itself are inactive"])
  -- the parent node must have been updated
  when (tstamp nd <= tstamp self) $ fail (unwords ["dependency violation!",
      "node", show (Mem.addr ns), "with tstamp", show $ tstamp self,
      "depends on",show (Mem.addr ndr),
      "whose tstamp", show . tstamp $ nd])
  let dist = f (nref_val ndr)
  let self' = case dist of
        -- Dirac is treated specially, see the note at the end of the file.
        Dirac v -> self{val = v,tstamp=tnow}
        -- don't update the tstamp if only LL changes
        -- !!! Don't forget to update the resampler as well!
        Resampleable (Dist uns samp) ->
          self{ll = uns (val self), resampler = samp}
        Observed _ uns ->
          self{ll = uns (val self)}
  return (Mem.modify_ref self' ns,Set.empty)

-- Create a node that depends on two nodes
with_node2 :: Show b =>
              (NodeRef a1, NodeRef a2) -> ((a1,a2) -> DistK k b) -> TraceM b
with_node2 (ndr1,ndr2) f = do
  let dist = f (nref_val ndr1,nref_val ndr2)
  nnew <- create_node (Just $ doupdate (ndr1,ndr2)) dist
  new_dependency ndr1 (Mem.addr nnew)
  new_dependency ndr2 (Mem.addr nnew)
  return nnew
 where
   -- The update never resamples. The val may change because it depends
   -- on the node value, and LL may also change even if val stays the same.
 doupdate (ndr1old,ndr2old) nsold = do
  ns   <- refresh_nref nsold                -- node being updated
  ndr1 <- refresh_nref ndr1old              -- node on which we depend
  ndr2 <- refresh_nref ndr2old              -- node on which we depend
  let nd1  = Mem.dref ndr1
      nd2  = Mem.dref ndr2
      self = Mem.dref ns
  tnow <- fmap now get
  when (tstamp nd1 <= tstamp self && tstamp nd2 <= tstamp self) $
    fail (unwords ["dependency violation!",
            "node", show (Mem.addr ns), "with tstamp", show $ tstamp self,
            "depends on",show [Mem.addr ndr1, Mem.addr ndr2],
            "whose tstamps", show [tstamp nd1, tstamp nd2]])
  let dist = f (nref_val ndr1,nref_val ndr2)
  let self' = case dist of
        Dirac v -> self{val = v,tstamp=tnow}
        -- don't update the tstamp if only LL changes
        -- !!! Don't forget to update the resampler as well!
        Resampleable (Dist uns samp) ->
          self{ll = uns (val self), resampler = samp}
        Observed _ uns ->
          self{ll = uns (val self)}
  return (Mem.modify_ref self' ns,Set.empty)

-- Sample programs
ex1 =
  with_node_none (uniformS 0 1) `next` ( \c ->
  with_node_none (bernS 0.5) `next` (\d ->
  with_node c (\cv -> bernS cv)  `next` (\e ->
  with_node2 (e,d) (\ (ev,dv) -> diracS (ev && dv)))))

ex1r = mcmc 7 ex1

prog_mult_conditions c1 c2 = 
  with_node_none (betaS 1 1) `next` (\b ->
  with_node b (\bv -> conditioned c1 (bernS bv)) `next` (\_ ->
  with_node b (\bv -> conditioned c2 (bernS bv)) `next` (\_ ->
  return b)))

exmcr = mcmc 7 (prog_mult_conditions True False)

exmcr1 = sum $ mcmc 1000 (prog_mult_conditions True False)

{-

ex2 =
  dist (uniform 0 1) `next` (\c ->
  dist (bern 0.5) `next` (\d ->
  (if d then dist (bern c) else dist (bern (1-c)) `next` xxx) `next` (\e ->
  return (e && d))))

prog_test1 =  do
  c <- unconditioned $ bern 0.5 
  d <- if c then unconditioned (bern 0.5) >> unconditioned (normal 0 1)
       else unconditioned $ beta 1 2
  conditioned (normal (if c then 20 else 10) 1)
  unconditioned $ normal d 0.1

-}

{-
  Branching

Branching (if-expressions) are subtle: when the value to branch upon
changes (because of re-sampling, for example), the nodes in one branch
of the if-expression should be deleted, and the nodes from the other
branch added. We also have to account for the changes in the size of the
node framework in the acceptance ratio.

Since adding and removing nodes is quite a chore, when the
if-expression is first executed (when the network is created), the
nodes in both brances are created. The nodes in the inactive branch
are deactivated. When the if-condition changes, we activate the nodes
in one branch and deactivate the other branch -- keeping track of
sizes for the changed branches.

Modulo keeping track of LL, deactivation is an optimization. We can in
principle keep the nodes in both IF braches as they are, updating them
when the nodes they depend upon change. The exit node (see below) will
choose, as the value of the entire IF expression, the value of one of
the two branches depending on the current value of the IF test
expression. However, recomputing the nodes in the IF branch whose value
will be ultimately ignored is not good for performamce. Therefore,
we mark the nodes in the inactive branch as inactive, and delay their
updates until they are activated. We mark only those nodes inactive that
are created in the branch itself, rather than those shared with other
nodes. Consider

  do
  c <- bernN (Val 0.5)
  d <- ...
  if_ c (return d)
    (do
      e <- ...
      f <- return d -- diracN d is the same as return d here
      diracN (liftN2 (+) e f))

Here, THEN branch in the IF statement is a single node d shared with
the rest of the program. This node will not be activated/deactivated when
the IF branch changes. The ELSE branch has two nodes, one of them is created
in the branch and the other, f, is an alias of d. Again, only e node
is subject to activation/deactivation.

When activating/deactivating, keep the nested branches in mind:

  do
  c <- bernN (Val 0.5)
  if_ c (do
         d <- ...
         if_ d e2 e3
        )
        ...

Suppose initially both c and d are true; therefore d and e2 are active and
e3 is inactive. Suppose that c is resampled to false. We have to deactivate
the true branch of if_ c, that is, nodes d, e2 and e3. But e3 was already
inactive. Therefore, we increase the deactivation count of d.
         
We compile the expression
   if test then-nodes else-nodes
into the following DAG:
   -- the entry node for the test-condition
   -- then-nodes
   -- else-nodes
   -- the exit-node, depending on the entry node and the final nodes
      of then-nodes and else-nodes.
The exit node's value is the value of the then-branch or the
else-branch, depending of the value of the test-node. The exit node is
the node that represents the if-expression.

Activation/deactivation per se does not change the LL of the trace.
Again, deactivation is an optimization, which conveptually just
freezes the updates nodes. Therefore, switching branches per se does
not change the LL. The activated nodes may get updated, and that
can change LL however. Conditioning, however, is very different.
-}

ifnode :: Show a => NodeRef Bool -> TraceM a -> TraceM a -> TraceM a
ifnode entry th el = do
  -- When a node is resampled, its update procedure is not run
  -- For the entry node, we need the update procedure to run. So,
  -- entry node should not be resampleable
  get >>= \s -> when (Mem.addr entry `elem` sampleable s) $
      fail "Entry node for if should not be sampleable"
  let testv = nref_val entry
  let (active,inactive) = if testv then (th,el) else (el,th)
  added_old <- fmap added get
  -- Instantiate the inactive branch
  inactive_nr    <- inactive
  added_inactive <- fmap added get
  let inactive_nodes = Set.difference added_inactive added_old
  -- inactive_nodes may be an empty set, if all its nodes are shared
  -- with the rest of the program. In the sample program in the comments
  -- above, the THEN branch has no unshared nodes.
  -- Deactivate and backtrack the branch
  deactivate inactive_nodes  -- which rolls back llcum
  -- Instantiate the active branch
  active_nr    <- active
  added_active <- fmap added get
  let active_nodes = Set.difference added_active added_inactive
  -- Create the exit node
  let thel_nr = if testv then (active_nr,inactive_nr)
                         else (inactive_nr,active_nr)
  exit <- create_node
          (Just $ update_exit entry thel_nr)
          (diracS $ nref_val active_nr)
  let exit_addr = Mem.addr exit
  new_dependency entry exit_addr
  new_dependency active_nr exit_addr
  new_dependency inactive_nr exit_addr
  debugM ["\nmaking the ifnode: entry node", show entry,
          "exit node", show (Mem.addr exit),
          "active branch", show active_nodes,
          "inactive branch",show inactive_nodes]
  -- add to the update of the entry node
  let thel_ns = if testv then (active_nodes,inactive_nodes)
                         else (inactive_nodes,active_nodes)
  modify (\s -> s{nodes =
     let n  = Mem.dref entry
         Just prev_upd = update n
         n' = n{update = Just (update_entry prev_upd thel_ns)}
     in snd $ Mem.store (Mem.modify_ref n' entry) (nodes s)})
  return exit
 where
   -- Deactivate nodes; unless the node was already inactive,
   -- remove its (conditioning weight) contribution from llcum.
   -- We look only at Conditioned nodes, that is, nodes
   -- which weight the distribution (with weight < 1).
   -- Nodes without conditioning have weight 1
   -- That is, normal nodes correspond to probability distribution
   -- OTH, conditioning (observation) nodes correspond to
   -- a subprobability distribution
   deactivate :: NodeSet -> MCMCM ()
   deactivate ns = do
     s <- get
     let (inactive', _, llc, inactivated) =
           Set.foldl' deactivate1 (inactive s, nodes s, 0, 0) ns
     put s{inactive = inactive', llcum = llcum s - llc,
           card_change = card_change s - inactivated}
    where
      deactivate1 (inactive,nodes,llc,inactivated) na =
        case M.insertLookupWithKey (const (+)) na 1 inactive of
          (Nothing,inactive) ->
            let (lln,r) = Mem.locate na nodes $ \nr ->
                  let n = Mem.dref nr
                      w = case kind n of
                            KObserved -> ll n
                            _         -> 0
                  in (w,resampleable n) in
            debug (unwords ["deactivating:", show na, "ll", show lln]) $
            (inactive,nodes,llc+lln,
             if r then inactivated+1 else inactivated)
            -- was already inactive
          (_,inactive) -> (inactive,nodes,llc,inactivated)

   -- Activate nodes; if the node becomes active (its deactivation count
   -- goes to 0), remove it from the inactive map and
   -- add its conditioned LL to llcum.
   -- We count only Observed nodes in the LL computation (see the comment in
   -- deactivate)
   activate :: NodeSet -> MCMCM ()
   activate ns = do
     s <- get
     let (inactive', _, llc, activated) =
           Set.foldl' activate1 (inactive s, nodes s, 0, 0) ns
     put s{inactive = inactive', llcum = llcum s + llc,
           card_change = card_change s + activated}
    where
      activate1 (inactive,nodes,llc,activated) na =
        case M.updateLookupWithKey updater na inactive of
          -- becoming active
          (Just 1,inactive) ->
            let (lln,r) = Mem.locate na nodes $ \nr ->
                  let n = Mem.dref nr
                      w = case kind n of
                            KObserved -> ll n
                            _         -> 0
                  in (w,resampleable n) in
            debug (unwords ["activating:", show na, "ll", show lln]) $
            (inactive,nodes,llc + lln,
             if r then activated+1 else activated)
            -- still inactive
          (Just _,inactive) -> (inactive,nodes,llc,activated)
            -- (Nothing,_) cannot happen: the node must have
            -- been present in the update map
      updater k 1 = Nothing
      updater k n | n > 1 = Just (n-1)
      -- inactivation count must be positive!
      
   -- modify all nodes in a set
   modify_nodes :: (forall a. Node a -> Node a) -> NodeSet -> MCMCM ()
   modify_nodes f = Set.foldr' ((>>) . go) (return ())
    where
      go na = modify (\s -> s{nodes = Mem.modify na (nodes s) f})

   -- the update function for the entry node
   update_entry prev_update (ths,els) nsold = do
     let testv_old = nref_val nsold
     pu@(ns,deps) <- prev_update nsold
     let testv = nref_val ns
     if testv_old == testv then return pu  -- the condition hasn't changed
        else switch_branches pu (if testv then (ths,els) else (els,ths))
             
   switch_branches (ns,deps) (newactive,newinactive) = do
     debugM ["branch switch, from:", show newinactive,"to",
             show newactive]
     deactivate newinactive
     activate newactive
     s <- get
     let newdeps = Set.intersection newactive (delayed_updates s)
     put s{delayed_updates = Set.difference (delayed_updates s) newdeps}
     return (ns,Set.union deps newdeps)
       
   -- the update function for the exit node
   update_exit entryold (th,el) nsold = do
     ns    <- refresh_nref nsold                -- node being updated
     entry <- refresh_nref entryold             -- test's node
     let testv  = nref_val entry
         active = if testv then th else el
     ndr <- refresh_nref active
     let nd   = Mem.dref ndr
         self = Mem.dref ns
     tnow <- fmap now get
     -- the parent nodes must have been updated
     when (tstamp (Mem.dref entry) <= tstamp self &&
           tstamp nd <= tstamp self) $
       fail $ unwords ["the parents of the exit node", show (Mem.addr ns),
                       "namely", show [Mem.addr entry, Mem.addr ndr],
                       "must have been updated"]
     let self' = self{val = nref_val ndr,tstamp=tnow}
     return (Mem.modify_ref self' ns,Set.empty)



exgrass =
  with_node_none (bernS 0.3) `next` (\rain ->
  with_node_none (bernS 0.5) `next` (\sprinkler ->
  with_node rain diracS `next` (\rain' ->
  with_node sprinkler diracS `next` (\sprinkler' ->
  ifnode rain' (with_node_none (diracS 0.9))
    (ifnode sprinkler' (with_node_none (diracS 0.8))
                      (with_node_none (diracS 0.1)))
  ))))

_ = mcmc 10 exgrass
-- [0.9,0.9,0.9,0.9,0.8,0.1,0.1,0.1,0.1,0.8]

exgrass' =
  with_node_none (bernS 0.3) `next` (\rain ->
  with_node_none (bernS 0.5) `next` (\sprinkler ->
  with_node rain diracS `next` (\rain' ->
  ifnode rain' (with_node_none (diracS 0.9))
    (with_node sprinkler diracS `next` (\sprinkler' ->
    (ifnode sprinkler' (with_node_none (diracS 0.8))
                      (with_node_none (diracS 0.1)))
  )))))
  
_ = mcmc 10 exgrass'
-- [0.9,0.9,0.9,0.9,0.8,0.1,0.1,0.1,0.1,0.8]

-- Branching tests
exbr2 = with_node_none (categoricalS [(1,0.5),(2,0.5)]) `next` (\x ->
        with_node x (\v -> diracS (v == (1::Int))) `next` (\xtest ->
        ifnode xtest
          (with_node_none (categoricalS [(10,0.5),(11,0.5)]))
          (with_node_none (categoricalS [(20,0.5),(21,0.5)]))
        ))

_ = mcmc 20 exbr2

exbr3 = with_node_none (categoricalS [(1,0.5),(2,0.5)]) `next` (\x ->
        with_node x (\v -> diracS (v == 1)) `next` (\xtest ->
        ifnode xtest
          (return x)
          (with_node_none (categoricalS [(20,0.5),(21,0.5)]))
        ))

_ = mcmc 20 exbr3



-- ------------------------------------------------------------------------
-- The MCMC algorithm itself

-- The main MCMC function, constructing the list of answers

-- We can be constructing the lazy list (List m a). It seems
-- just limiting the length of the chain should be enough for now.

rejectedLL :: LL -> Bool
rejectedLL = isInfinite -- that is, -Infinity

mcmc :: Integer -> TraceM a -> [a]
mcmc limit prog =
  let g0 = mkStdGen 17
      (trace,state,limit') = initial_step limit g0 prog
  in debug (unwords [
        "Trace constructed:",
        show . Mem.size . nodes $ state, "nodes;",
        show . inactive $ state, "inactive;",
        show . sampleable $ state, "sampleable nodes; dependencies\n",
        show $ deps state,
        "Card diff", show (card_change state)] ++
        (unlines . reverse . debug_msg $ state)) $
     if limit' == 0 then [] else
       transition trace state{added=Set.empty} limit
   
-- Run the program initially and construct the trace
initial_step' :: StdGen -> TraceM a -> (NodeRef a, MCState)
initial_step' g pgm = runState pgm init_state
 where
  init_state = MCState {
    seed=g, now=1,
    nodes=Mem.empty, deps=M.empty, inactive=M.empty,
    llcum=0, delayed_updates=Set.empty,
    card_change=0,
    sampleable=[], added=Set.empty, debug_msg=[]}

-- If the rejection-sampling conditioning is present (on arbitrary boolean
-- expression), the initial trace may be rejected.
-- If so, repeat up to limit times to construct the good initial trace.
initial_step :: Integer -> StdGen -> TraceM a -> (NodeRef a, MCState, Integer)
initial_step limit g pgm = go limit $ initial_step' g pgm
 where
   go 0 (nr,state) = (nr,state,0)
   go limit (_,state) | rejectedLL (llcum state) =
     go (limit-1) $ initial_step' (seed state) pgm
   go limit (nr,state) = (nr,state,limit)



-- Build the Markov chain, by updating the trace

transition :: NodeRef a -> MCState -> Integer -> [a]
transition trace st 0 = []
transition trace st limit =
  let vold  = nref_val trace
      ((accept, trace'),st') = runState transit st{debug_msg=[]}
  in (vold:) $
    debug (unlines . reverse . debug_msg $ st') $
    if accept then transition trace' st' (limit-1)
    else debug "REJECTED" $
         transition trace st{seed=seed st'} (limit-1)
 where
  transit = do
    -- advance the tstamp and zero out llcum so it will accumulate
    -- the differences in LL from the previous trace
    -- After the first step, card_change records the number of
    -- inactive nodes at the beginning. So,
    -- length (sampleable) + card_change is the size of the chain,
    -- the number of active sampleable nodes.
    modify $ \s -> s{now = now s + 1,llcum=0}
    carddiff_old <- fmap card_change get
    sampleables  <- fmap (length . sampleable) get
    -- change the trace somehow
    (changed,fwd,bwd) <- resample
    -- propagate the change
    propagate_update $ debug ("\nchanged " ++ (show changed)) $ changed
    trace'   <- refresh_nref trace
    lldiff   <- fmap llcum get              -- llnew - llold
    carddiff <- fmap card_change get
    debugM ["fwd", show fwd, "bwd", show bwd, "lldiff", show lldiff,
            "carddiff", show carddiff]
    if rejectedLL lldiff then return (False,trace') else do
      -- compute the acceptance ratio (actually, its log)
      let acceptance_ratio = 
           lldiff -- llnew - llold
           + bwd - fwd
           + (if carddiff == carddiff_old then 0 else
               log( fromIntegral (sampleables + carddiff_old) /
                    fromIntegral (sampleables + carddiff)))
          -- size resampleable + added, size resampleable + deleted
          -- + log (fromIntegral dbSize) - log (fromIntegral $ M.size db2)
          -- + llStale - llFresh -- ll of deleted and added
      debugM ["acceptance ratio", show acceptance_ratio]
      accept <- if acceptance_ratio >= 0 then return True
                 -- TODO: if acceptance_ratio is too negative, reject
              else do
                u <- randomS (0::Double,1)
                debugM ["u", show u]
                return $ u < exp acceptance_ratio
      return (accept,trace')

   
-- Modify the trace somehow and compute the forward and backward
-- LogLikelihood associated with the change.
-- Currently, our proposal for the change is to pick one resampleable
-- node and reseample it.
-- TODO: Make a way of passing user-provided proposal distributions
resample :: MCMCM (UpdateQueue,Double,Double)
resample = do
  resampleable <- fmap sampleable get
  -- TODO: use the elevator sampling to save on the list traversal
  -- The elevator sampling will also avoid recursion if we happen to
  -- hit an inactive node
  nref <- fmap (resampleable !!) $ randomS (0,length resampleable - 1)
  -- xx <- randomS(0,2048::Int)
  -- nref <- fmap (resampleable !!) $ (return $ if xx > 1024 then 0 else 1)
  s <- get
  if M.member nref (inactive s) then resample
    else do
    Mem.locate nref (nodes s) $ \nr -> do
      let n = Mem.dref nr
          bwd = ll n
          -- Must be resampleable
          (v,fwd,g') = resampler n (seed s)
      -- Use modify rather than get, to avoid clubber the current state!!
      modify $ \s -> s{seed=g'}
      debugM ["New gen", show nref," node resampled", show n]
      q <- modify_node $ Mem.modify_ref n{val=v,ll=fwd,tstamp=now s} nr
      return (q,fwd,bwd)


{-
A note on re-sampling and dirac

p1 = do 
     a <- uniform 0 1
     bern a

Suppose when the program first run, a is sampled to 0.5 and bern is
sampled to false. When updating the chain, suppose we chose to
resample from the uniform distribution. Suppose the new sample is 0.7.
We do not resample from bern. See the original Metropolis
implementation: the existing XRP keeps its existing value.  So, the
program still returns false. However, since a has changed, this false
is taken from bern 0.7, which changes the overall LL and hence the
acceptance ratio. Is this correct?

Suppose I now change the program slightly

p2 = do 
     a <- uniform 0 1
     dirac a

Again, suppose that initially uniform is sampled at 0.5, hence the
program returns 0.5. Suppose to update the chain, we again pick
uniform for resampling, and resample it to 0.7. What happens then?

If we view dirac as any other distribution (like bern in p1), we
should not resample form it.  We maintain the previous sample 0.5 but
now the distribution becomes dirac 0.7, which makes LL to be -infinity
and so the proposal is rejected. So, for the entire chain, our program
will keep producing 0.5. The chain will be [0.5, 0.5,...].

That is certainly undesirable. Theoretically, our chain fails to mix
if all proposals are rejected. Practically, we want dirac to act as a
left unit so that p2 becomes equivalent to just (uniform 0 1).

The way to obtain the desired behavior is to resample from dirac too.
So, when x changes, (dirac x) should be resampled. Such a policy in
effect corresponds to a proposal that resamples in p2 *both* from
uniform and dirac; furthermore, it resamples from dirac in such a way
that the new sample magically matches the new x. In other words, for
program p2 a proposal that only resamples from a single node, uniform
or dirac, will always be rejected. To get the chain to mix, we have to
propose a correlated resampling from both nodes, uniform and dirac.
Making dirac to resample whenever its argument changes in effect
accomplishes this multiple-node correlated resampling.

-}

{-
diractest = mcmC 5 $ do 
     a <- bernN (Val 0.5)
     diracN a
-}
diractest = mcmc 10 $
  with_node_none (bernS 0.5) `next` (\a ->
  with_node a (\a -> diracS a))
-- The result should show both True and False, in the same proportion

{-
  A note on computing acceptance ratio (actually, its log)

The original Metropolis.hs computed the acceptance ratio according
to the formula

      a = llTotal - llTotal_old
          + rvs - fwd
-- XXX should dbsize include observed nodes?
          + log (fromIntegral dbSize) - log (fromIntegral $ M.size db2)
          + llStale - llFresh

Here, llTotal is the sum of LL of the nodes in the new chain,
      llTotal_old   the same for the old chain
      rvs and fwd -- reverse/fwd proposal LL
      db2    -- the size of the new chain
      dbsize -- the size of the old chain
      llFresh -- LL for the newly added nodes
      llStale -- LL for the nodes prsent only in the old chain
                 (that is, removed nodes)

Here is how we modify the formula for the incremental computation case:

Suppose U is the set of nodes that are not affected by the update,
M is the set of modified nodes, A is the set of added nodes
(by newly taken conditional branches) and D is the set of deleted nodes
(nodes in inactive branches). We write Mold for nodes in M before the update.

The old chain was the set  U+Mold + D, the new chain is the set U+M+A
Let ll(S) be the sum of LL of the nodes in the set S and card(S) be
the cardinality of the set.

Therefore, llTotal_old = ll(U) + ll(Mold) + ll(D),
           llTotal     = ll(U) + ll(M) + ll(A)
           llFresh     = ll(A)
           llStale     = ll(D)

So, the formula for the log of the acceptance ratio is

           ll(U) + ll(M) + ll(A)
         - ll(U) - ll(Mold) - ll(D)
         + rvs - fwd
         + log( card(U+M+D) / card(U+M+A) )
         + ll(D) - ll(A)

or
           ll(M) - ll(Mold)
         + rvs - fwd
         + log( card(U+M+D) / card(U+M+A) )

We accumulate ll(M) - ll(Mold) when we update a node.

In the presence of branches, things become very tricky. See the
correspondence on the PPAML list in Sep 2014.
Briefly, the formula, due to Wingate et al.,
depends on identifying so called fresh and stale nodes. This identification
depends on a particular labeling scheme for nodes. A different labeling scheme
gives different stale/fresh nodes, leads to a different acceptance ratio
and to a different chain (which may or may not mix). There is no rule
how to select a good labeling scheme and which labeling scheme is good.

In general, it seems to me that the whole labeling business is beside the
point and is simply an artefact of Wingate et al. approach. Here is the
explanation and the justification for the new approach.

Consider the program
p1  = do
      c <- uniformN (Val 0) (Val 1.0)
      d <- bernN (Val 0.5)
      e <- bernN (Val 0.5)
      ...

As written it has nodes c, d, and e. But we might as well consider it to be

p1'  = do
      c <- uniformN (Val 0) (Val 1.0)
      (d,e) <- twobernN (Val 0.5) (Val 0.5)
      ...

where twobernN is the distribution that corresponds to two independent
bern distribution. In that program, there are only two nodes (not counting
...). That is, any composition of nodes can be considered a node,
and the entire program can be just a single node. This conclusion holds
for programs 

p2  = do
      c <- uniformN (Val 0) (Val 1.0)
      d <- uniformN (Val 0) (Val 1.0)
      e <- bernN d
      ...

p3  = do
      c <- uniformN (Val 0) (Val 1.0)
      d <- uniformN (Val 0) c
      e <- bernN d
      ...

In both p2 and p3, nodes c and d can be merged to a node that corresponds
to a more complex distribution. The conclusion also holds if we do
the conditioning.

These examples show that the notion of a node is arbitrary; what we take
a node is determined by mere convenience, by which distributions we can
conveniently sample from and condition upon. Therefore, the notion of node
address is just as arbitrary.

So, when it comes to computing the acceptance ratio for a program
with branches, we take

p4 = do
     c <- bernN (Val 0.5)
     if_ c (do e1 e2 ...)
           (do e3 e4 e5 ...)

to be

p4' = do
      c <- bernN (Val 0.5)
      if_ c e_then e_else

That is, we merge (at least for the purpose of presentation) e1, e2 ...
into one node e_then, whose ll is the sum of ll of constituents. Ditto
for e_else.

When we switch the branches, we remove the ll of one branch from
llcum and add ll of the other branch. In our terms, we remove the ll
of the deactivated nodes and add ll of the activated nodes.

This algorithm is resilient with respect to adding replacing a node e
with e >>= diracN -- the desired behavior.


-}


-- ------------------------------------------------------------------------
-- Old code, kept for reference

{-

-- the first component is the LogLikelihood of the trace
-- The second is the LogLikelihood of the newly introduced
-- choices. These data are used to compute the acceptance ratio
type LL2 = (LL,LL)

{-

Shortcomings of this implementation

* uses parent-conditional sampling for proposal distribution
* re-evaluates entire program at every sample
* lacks way to block sample groups of variables

-}

-- and what does XRP stand for?
            -- XRP always contains the LL of the current sample with respect
            -- to its distribution
data XRP where
  XRP :: Typeable e => (Density e, LL, Dist e) -> XRP

type Visited = Bool
type Observed = Bool


type Subloc = Int
type Name = [Subloc]
data DBEntry = DBEntry {
      xrp  :: XRP, 
      llhd :: LL, 
      vis  :: Visited,
      observed :: Observed }
type Database = M.Map Name DBEntry

data SamplerState cnds g =
  S { ldb :: Database, -- ldb = local database
      -- (total likelihood, total likelihood of XRPs newly introduced)
      llh2 :: {-# UNPACK #-} !LL2,
      cnds :: cnds, -- conditions left to process
      seed :: g }

-- A sampling computation that produces the result of type a, needs
-- cin conditioning environment, leaving cout for the rest
type Sampler cin cout a =
  forall g. RandomGen g => SamplerState cin g -> (a, SamplerState cout g)

-- The standard State parameterized monad
-- The environment monad; the environment is the trace coordinate
-- (address of the updateXRP node in the trace)
newtype Measure ci co a =
  Measure {unMeasure :: Name -> Sampler ci co a}

return_ :: a -> Measure cn cn a
return_ x = Measure $ \_ -> sreturn x

-- Running the program is performing the sequence of updateXRP,
-- which update the database (the state of the computation).
-- The Name (the first argument) is the `address', location of the
-- the current XRP along the sequence of updateXRP.
-- We may call the sequence of XRP the `trace'.
-- The database may contain the previous trace.
-- The new trace may have a different dist' (because the parameters
-- of dist' may depend on already computed values, which depend
-- on the previous choices).
updateXRP :: Typeable a =>
             Name -> Maybe (Density a) -> Dist a -> Sampler ci ci a
updateXRP n _ dist' s@(S {ldb = db}) |
      Just (DBEntry (XRP (xb, _, _)) _ _ ob) <- M.lookup n db =
      -- The node with this name was already in the trace.
      -- We sure hope that if this db entry corresponded to an observation
      -- in an earlier trace, it corresponds to exactly the same
      -- observation in the current trace! Nothing enforces this though!
        let Just dist'' = cast dist'
            l' = logDensity dist'' xb
            -- XRP always contains the LL of the current x with respect
            -- to the distribution dist
            -- We have to do reweighting anyway.
            -- -- Rob: I feel you always have to reweight
            -- -- xb whether in obs or not. It probably makes sense
            --    to move that out.
            d1 = M.insert n (DBEntry (XRP (xb,l',dist'')) l' True ob) db
            Just x = cast xb
        in (fromDensity x,
            s {ldb = d1,
               llh2 = updateLogLikelihood (l',0) (llh2 s)})

      -- The node with the 'name' did not exist in the previous trace;
      -- This is the new node to compute.
      -- This is typical on the new run, or when an earlier choice
      -- causes a new conditional branch to be executed.
      -- We either make a choice, or use the observation if the choice
      -- was observed (conditioned upon).
updateXRP n obs dist' s@(S{ldb = db, seed = g}) =
  let (xnew2, l, g2) = case obs of
             Just xnew -> (xnew, logDensity dist' xnew, g)
             Nothing   -> distSample dist' g
      d1 = M.insert n (DBEntry (XRP (xnew2, l, dist')) l True (isJust obs)) db
  in (fromDensity xnew2,
      s {ldb = d1,
         llh2 = updateLogLikelihood (l,l) (llh2 s),
         seed = g2})

updateLogLikelihood :: LL2 -> LL2 -> LL2
updateLogLikelihood (llTotal,llFresh) (l,lf) = (llTotal+l, llFresh+lf)

{-
factor :: LL -> Measure ()
factor l = Measure $ \ _ -> \ s ->
   let (llTotal, llFresh) = llh2 s
   in ((), s {llh2 = (llTotal + l, llFresh)})
XXX

-- Does this program make sense?
-- do c <- bern 0.5
      d <- conditioned (bern 0.7) c
      return d

-- conditioned :: Dist a -> Observed a -> Measure a

-- The discussion showed that conditioned could be correct
-- if expressed in terms of disintegrate (or, better,
-- density) -- see Syntax3.hs

conditioned :: Dist a -> a -> Measure a
conditioned d obs = condition (do
  -- y <- unconditioned d
  x <- unconditioned d
  return (x,x)) obs

-- XXX
-- It is not clear if this mere rejection (returning log 0)
-- correctly adjusts the weights in the successful case.
-- That is, if
--  given m :: Measure (a,b)
--     m >>= \_ ->  return ()
-- is the same as
--     m >>= \ (_,b) -> condition m b >>= \_ -> return ()
-- Indeed, the empirical test showed the normalization problem.

condition :: Eq b => Measure (a, b) -> b -> Measure a
condition (Measure m) b' = Measure $ \ n ->
    let comp a b s |  a /= b = s {llh2 = (log 0, 0)}
        comp _ _ s =  s
    in sbind (m n) (\ (a, b) s -> (a, comp b b' s))
-}

-- Q.
-- If the trace diverges, can it converge?
-- That is, if the new trace takes a new branch, can it merge
-- with the old trace later again?
-- A.
-- I think we can recover the old trace later, but we don't
-- keep track of that, it will just so happen to be a new trace
-- with the exact same list of random choices

-- do
--  c <- bern 0.5 
--  d <- if c then m1 >> m2 else m3
--  condition (m4 c)
--  m5 d

-- if m1 and m2 are both conditioned, we are in trouble!
-- Here (assuming all do-binds are right-associated).
--   flip: address [0,0]
--   if c:false
--    m1: [0,0,1,0]  m2: [1,0,1,0]
--    m4: [1,1,0]
-- But all bets are off if the compiler does reassociation!

---- How are these programs not equivalent under association?
---- My understanding is the programs with different association
---- should be equivalent probability distributions

-- Q. Hmm, bind is non-associative!
-- This is not a monad (regardless of parameterization).
-- This is a potential problem, if the second execution reassociates
-- binds. We need CPS, to ensure right-associativity and canonical
-- addressing.
-- A. Why is canonical addressing so crucial?


bind :: Measure cin cm a -> (a -> Measure cm cout b) -> Measure cin cout b
bind (Measure m) cont = Measure $ \n ->
    sbind (m (0:n)) (\ a -> unMeasure (cont a) (1:n))

conditioned :: Typeable a => Dist a -> Measure (Maybe (Density a),c) c a
conditioned dist = Measure $ \n -> 
    \s@(S {cnds = (cond,conds) }) ->
        updateXRP n cond dist s{cnds = conds}



-- Run the program from the beginning to the end, using the database
-- (initially empty) that recorded the choices from a previous
-- execution
traceUpdate :: RandomGen g => Measure cnds () a -> Database -> cnds -> g
            -> (a, Database, LL, LL, LL, g)
traceUpdate (Measure prog) d cds g = do
  -- let d1 = M.map (\ (x, l, _, ob) -> (x, l, False, ob)) d
  let d1 = M.map (\ s -> s { vis = False }) d
  let (v, S d2 (llTotal, llFresh) () g1) = (prog [0]) (S d1 (0,0) cds g)
  let (d3, stale_d) = M.partition vis d2
  let llStale = M.foldl' (\ llStale' s -> llStale' + llhd s) 0 stale_d
  (v, d3, llTotal, llFresh, llStale, g1)

initialStep :: Measure cnds () a -> cnds ->
               IO (a, Database, LL, LL, LL, StdGen)
initialStep prog cds = do
  g <- getStdGen
  return $ traceUpdate prog M.empty cds g


transition :: (Typeable a, RandomGen g) => Measure cnds () a -> cnds
           -> a -> Database -> LL -> g -> [a]
transition prog cds v db ll g =
  let dbSize = M.size db
      -- choose an unconditioned XRP (not associated with an
      -- observation to condition upon)
      (_, uncondDb) = M.partition observed db
      (choice, g1) = randomR (0, (M.size uncondDb) -1) g
      (name, (DBEntry xd _ _ ob))  = M.elemAt choice uncondDb
      (db', _, fwd, rvs, g2) = resample name db ob xd g1
      (v', db2, llTotal, llFresh, llStale, g3) = traceUpdate prog db' cds g2
      a = llTotal - ll
          + rvs - fwd
-- XXX should dbsize include observed nodes?
          + log (fromIntegral dbSize) - log (fromIntegral $ M.size db2)
          + llStale - llFresh
      (u, g4) = randomR (0 :: Double, 1) g3 in

  if (log u < a) then
      v' : (transition prog cds v' db2 llTotal g4)
  else
      v : (transition prog cds v db ll g4)


-}
