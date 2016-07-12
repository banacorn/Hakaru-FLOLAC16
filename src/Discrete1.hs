{-# Language NoMonomorphismRestriction, GADTs #-}

-- Graphical models and the inference on them
-- Discrete case

module Discrete where

import qualified Data.Map as M
import System.Random

-- ------------------------------------------------------------------------
-- Representing distributions
-- (Tagless-final aproach)

-- running example: flipping a fair coin twice and obtaining
-- the probability the two outcomes are different

xor :: Bool -> Bool -> Bool
xor x y = not (x == y)

-- We need to represent graphical models like Grass and the two-coin model
-- Draw the model.
-- We need to formally define what the graphical model really means.
-- It means the joint distribution:
-- Pr(Coin1=x,Coin2=y,HeadTail=t) =
--  {chain rule}
-- Pr(Coin1=x) * Pr(Coin2=y,HeadTail=t|Coin1=x) =
--  {chain rule}
-- Pr(Coin1=x) * Pr(Coin2=y|Coin1=x) * Pr((x = not y)=t|Coin1=x,Coin2=y)
--  {conditional independence, from the graph}
-- Pr(Coin1=x) * Pr(Coin2=y) * Pr((x = not y)=t|Coin1=x,Coin2=y)


-- Let's define operations we need
type Prob = Double

-- First attempt

-- Suppose (d t) represents the probability distribution, in some yet
-- to be defined way. We parameterized it by the type of samples:
-- we really want to distinguish coin flip (producing Booleans) from
-- die roll or the normal distribution

class Dist' d where
  bern'  :: Prob -> d Bool

  -- To represent terms like Pr(t=(xor true false))
  -- which is 1 if t is indeed true, or 0 otherwise.
  -- Pr(a=x)
  dirac' :: a -> d a

  -- How to represent the conditional distribution Pr(X|P)?
  -- (for some parameters P)?
  -- Obviously, like functions P -> Pr(X)
  
  -- How to represent the chain law
  -- Pr(X=x,Y=y) = PR(X=x) * Pr(Y=y|X=x)
  chain' :: d a -> (a -> d b) -> d (a,b)

twocoins' = chain' (bern' 0.5) $ \x ->
            chain' (bern' 0.5) $ \y ->
            dirac' (xor x y)
-- what is the type?
-- How is the conditional independence expressed?

-- How to compute the probability if in two successive tosses,
-- there is one head, one tail?

-- Integrate out
-- Sum_x Sum_y Pr(Coin1=x,Coin2=y,HeadTail)
-- What this means...
-- [((False,(False,False)),0.25),
--  ((False,(True,True)),0.25),
--  ((True,(False,True)),0.25),
--  ((True,(True,False)),0.25)]


-- The summation is bothersome. There should be a better way.
-- Let's combine sampling with chain
-- Pr(Y=y) = Sum_x Pr(X=x,Y=y) = Sum_x Pr(X=x) * Pr(Y=y|X=x)
-- This has a nice ring to it: building a  new distribution from
-- others.

class Dist'' d where
  bern''  :: Prob -> d Bool

  dirac'' :: a -> d a

  chain'' :: d a -> (a -> d b) -> d b


twocoins'' = chain'' (bern'' 0.5) $ \x ->
             chain'' (bern'' 0.5) $ \y ->
             dirac'' (xor x y)


-- What is the inferred type?

-- We haven't lost anything

twocoins1'' = undefined

-- The notation is a bit ungainly though...

-- What a happy coincidence!

class Monad d => Dist d where
  bern :: Prob -> d Bool
  failure :: d a         -- we see the need for this when it comes to
                         -- conditioning

twocoins = do       
  x <- bern 0.5
  y <- bern 0.5
  return $ xor x y

twocoins1 = do       
  x <- bern 0.5
  y <- bern 0.5
  return $ (x,(y,xor x y))

-- This is just the syntactic sugar. GHC re-writes it into the form
-- twocoins' that we saw before. 

-- Again, what is the type of twocoins?

-- Let us implement another model, as an exercise:
-- die roll; throw n dice
-- Draw the model

class Dist d => Die d where
  die :: d Int   -- in Agda, Finite 6. But we do Int for now

-- dieroll n = undefined

-- _ = runExact (dieroll 3)

-- Implementing grass model

grass_fwd = do
  rain         <- bern 0.3
  sprinkler    <- bern 0.5
  grass_is_wet <- nor 0.9 0.8 0.1 rain sprinkler
  return grass_is_wet

-- noisy-or function
nor :: Double -> Double -> Double -> Bool -> Bool -> Dst Bool
nor strengthX strengthY noise x y =
  bern $ 1 - nnot (1-strengthX) x * nnot (1-strengthY) y * (1-noise)
  
-- noisy not function
nnot :: Num a => a -> Bool -> a
nnot p True  = p
nnot p False = 1

_ = runExact grass_fwd

-- Implement uniformly given bern
-- Demonstrate that the implementation is correct
-- Prove it
-- Point: recursive implementation

uniformly :: Ord a => [a] -> Dst a
uniformly = undefined

_ = runExact $ uniformly ([]::[Int])
_ = runExact $ uniformly [1]
_ = runExact $ uniformly [1,2]
_ = runExact $ uniformly [1..10]

{- Blood type example (simple version)

From PRISM documentation,
1.2 Basic probabilistic inference and parameter learning

``Let us pick up another example that models the inheritance mechanism
of human's ABO blood type. As is well-known, a human's blood type
(phenotype) is determined by his/her genotype, which is a pair of two
genes (A, B or O) inherited from his/her father and mother. For
example, when one's genotype is AA or AO (OA), his/her phenotype will
be type A. In a probabilistic context, on the other hand, we consider
a pool of genes, and let pa , pb and po denote the frequencies of gene
A, B and O in the pool, respectively ( pa + pb + po = 1). When random
mating is assumed, the frequencies of phenotypes, namely, PA , PB , PO
and PAB , are computed by Hardy- Weinberg's law [13]: PA = pa^2 + 2 pa
po , P B = pb^2 + 2 pb po , PO = po^2 and PAB = 2 pa pb.''

?- prob(bloodtype(a)).
Probability of bloodtype(a) is: 0.360507016168634

http://rjida.meijo-u.ac.jp/prism/download/prism22.pdf
-}

data Gene = GA | GB | GO
data BloodType = A | B | O | AB

{- Monty Hall Problem
https://en.wikipedia.org/wiki/Monty_Hall_problem

    Suppose you're on a game show, and you're given the choice of three doors:
    Behind one door is a car; behind the others, goats. You pick a door, say
    No. 1, and the host, who knows what's behind the doors, opens another door,
    say No. 3, which has a goat. He then says to you, "Do you want to pick door
    No. 2?" Is it to your advantage to switch your choice?
-}

monty_hall :: Bool -> Dst Bool
monty_hall = undefined

_ = runExact $ monty_hall False
_ = runExact $ monty_hall True

-- ------------------------------------------------------------------------
-- Conditioning

normalize :: PT a -> PT a
normalize pt = map (map_snd (/nf)) pt
 where nf = sum $ map snd pt
       
-- Eldest daughter puzzle
-- A family has exactly two kids, one of them is a girl.
-- What is the chance the older is a girl?

girl = undefined

-- _ = runExact girl
-- _ = normalize $ runExact girl

-- Fair coin tosses given arbitrarily biased coin (von Neumann trick)
-- Show/Prove the correctness
-- (recursion, rejection)

fair_coin :: Dst Bool -> Dst Bool
fair_coin c = undefined

_ = runExact $ fair_coin (bern 0.2)

-- Drunken coin example:
-- very large depth and very low probability of acceptance

-- Drunken coin example: coin flipping by a drunk.
-- Because the person flipping the coin is drunk, most of the time
-- the result of flipping is a lost/dropped coin

drunk_coin :: Dst Bool
drunk_coin = undefined

-- Compute AND of n tosses of the drunk coin 
-- dcoin_and n = 

-- _ = runExact $ dcoin_and 10
-- [(False,9.990234374999978e-11),(True,9.765624999999978e-14)]

-- Can we implement more efficiently?
-- (shorthand and) Why the False probability changes?


-- ------------------------------------------------------------------------
-- Implementation
-- It is quite tricky; and those who want to know the tricky bit,
-- can ask later...

-- I show the implementation that you haven't seen: it is fully
-- above the board and faithfully implements specification.

-- Probability Table
type PT a = [(a,Prob)]  -- Ord a

map_fst :: (a -> b) -> (a,c) -> (b,c)
map_fst f (x,y) = (f x,y)

map_snd :: (a -> b) -> (c,a) -> (c,b)
map_snd f (x,y) = (x,f y)


-- The representation of distributions
data Dst a where
  Single :: PT a -> Dst a
  Chain  :: PT b -> (b -> Dst a) -> Dst a

instance Functor Dst where
   fmap f (Single pt) = Single (map (map_fst f) pt)
   fmap f (Chain d k) = Chain d (fmap f . k)
  
instance Monad Dst where
  return x = Single [(x,1)]

  -- Exercise: why is this correct?
  Single []      >>= k = Single []
  Single [(x,1)] >>= k = k x    -- Nothing to sum over
  Single pt      >>= k = Chain pt k
  
  -- Exercise: justify this
  Chain pt k1 >>= k = Chain pt (\x -> k1 x >>= k)


-- The handler of the Dst effect: Exact inference
runExact :: Ord a => Dst a -> PT a
runExact (Single x) = x
runExact (Chain pt k) =
  M.toList . M.fromListWith (+) .
   concatMap (\ (x,p) -> map (map_snd (*p)) $ runExact (k x)) $ pt


instance Dist Dst where
  bern p  = Single [(True, p), (False,1-p)]
  failure = Single []
  
instance Die Dst where
  die = Single $ [ (x,1/6) | x <- [1..6] ]
  
_ = runExact twocoins

-- A different handler: sampling

runSample :: Int -> Dst a -> [a]
runSample n m = go (mkStdGen 17) n m
  where
    go g 0 _ = []
    go g n (Single []) = go g (n-1) m
    go g n (Single pt) =
      let (x,g1) = select g pt in x:go g1 (n-1) m
    go g n (Chain [] k) = go g (n-1) m
    go g n (Chain pt k) =
      let (x,g1) = select g pt in go g1 n (k x)

    select g [(x,1)] = (x,g)
    select g pt      =
      let (r,g1) = randomR (0::Double,1) g
          x = fst . head . dropWhile (\ (_,p) -> r > p) $
              scanl1 (\ (_,p1) (x,p2) -> (x,p1+p2)) pt
      in (x,g1)

_ = runSample 10 twocoins

_ = let n = 20 in (/ fromIntegral n) . fromIntegral . length . filter id $ runSample n grass_fwd

