-- Various simple examples of using Hakaru10

module Examples1 where

import Syntax
import Util
import Control.Applicative

-- Simple models
pbern = dist bern 0.4

pbern_run = mcmC 10 pbern

pnorm = dist normal 10 0.5

pdep1 = do
  x <- pnorm
  diracN (x+1)

-- Literally the same samples
pdep_run = mcmC 100 (dist normal 11 0.5) == mcmC 100 pdep1
-- True

pdepR = do
  x <- pnorm
  diracN x

pdepR_run = mcmC 100 pdepR

pdepR' = do
  x <- pnorm
  return x



pjoin = do
  x <- pbern
  y <- pnorm
  return (pair x y)

_ = mcmC 100 pjoin

pmarg = do
  x <- pbern
  y <- pnorm
  return y

pmarg_run = mcmC 100 pmarg

-- Fig 3 of Hur et al
-- double x;
-- int i = 0;
-- x ~ Gaussian(0, 1);
-- while (i < 10) do {
--   x ~ Gaussian(x, 3);
--   i = i+1;
-- }
-- return x;

-- Dependencies

phier = 
  (iterate (\m -> do {x<-m; dist normal x 3}) $ dist normal 0 1) !! 10

ph1 = do
  x <- dist normal 0 1
  dist normal x 3

phier_kl n =
  kl (tabulate 0.5 (mcmC n phier))
     (tabulate 0.5 (mcmC n (dist normal 0 (sqrt <$> 91))))
     

_ =  phier_kl 10

{-
*Examples1> phier_kl 1000
0.7862745014080637
*Examples1> phier_kl 10000
0.23618257021796316
*Examples1> phier_kl 100000
3.441636543766045e-2
*Examples1> phier_kl 1000000
2.1573977909591603e-3
-}

             
ph2 = do
  x <- dist bern 0.2
  dist bern ((\xv -> if xv then 0.1 else 0.7) <$> x)
-- 0.2 * 0.1 + 0.8*0.7 = 0.58

ph2r = length . filter id $ mcmC 10000 ph2
-- 5604

ph21 = do
  x <- dist bern 0.2
  y <- dist bern ((\xv -> if xv then 0.1 else 0.7) <$> x)
  return x

ph21r = length . filter id $ mcmC 10000 ph21
-- 2094  

-- Conditionals (mixing)

pcond1 = do
  x <- dist categorical (Val [(1,0.5), (2,0.5)])
  if_ ((==1) <$> x)
     (dist categorical (Val [(10,0.5), (11,0.5)]))
     (dist categorical (Val [(20,0.5), (21,0.5)]))

pcond1r = hist $ mcmC 10000 pcond1
-- fromList [(10,246),(11,222),(20,240),(21,292)]
-- fromList [(10,2481),(11,2394),(20,2565),(21,2560)]

pcond2 = do
  x <- dist categorical (Val [(1,0.5), (2,0.5)])
  if_ ((==1) <$> x)
     (return x)
     (dist categorical (Val [(20,0.5), (21,0.5)]))

pcond2r = hist $ mcmC 10000 pcond2
-- fromList [(1,500),(20,214),(21,286)]
-- fromList [(1,5000),(20,2321),(21,2679)]

pcond3 = do
  x <- dist categorical (Val [(1,0.5), (2,0.5)])
  if_ ((==1) <$> x)
     (return x)
     (do
       dist normal 0 1
       dist categorical (Val [(20,0.5), (21,0.5)]))

pcond3r = hist $ mcmC 10000 pcond3
-- fromList [(1,5011),(20,2507),(21,2482)]


-- Mixing model
mixng = do
  x <- dist normal 0 1
  if_ ((>0) <$> x)
           (dist normal 10 2)
           (dist gamma 3 (1/3))


mixng' = do
  x <- dist bern 0.5
  if_ x
           (dist normal 10 2)
           (dist gamma 3 (1/3))

_  =
 let n = 5000 in kl (tabulate 0.5 (mcmC n mixng)) (tabulate 0.5 (mcmC n mixng'))
{-
5000 samples 2.275866271810348e-2
-}
_  =
 let n = 5000 in kl (tabulate 0.5 (mcmC n mixng)) (tabulate 0.5 $ mcmC (n `div` 2) (dist normal 10 2) ++ mcmC (n `div` 2) (dist gamma 3 (1/3)))
{-
5000 samples 1.3563495645465825e-2
-}

-- Conditioning

-- coin bias estimation
biased_coin c1 c2 = do
  b <- dist beta 1 1
  dist (c1 `condition` bern) b
  dist (c2 `condition` bern) b
  return b

{- Type error
biased_coinX c1 c2 = do
  b <- dist beta 1 1
  dist (c1 `condition`  (c2 `condition` bern)) b
  dist (c2 `condition` bern) b
  return b
-}

{-
biased_coin_ill_typed c1 c2 = do
  b <- dist bern 0.5
  dist (c1 `condition` bern) b
  dist (c2 `condition` bern) b
  return b
-}

{-
biased_coin_ill_typed_too c1 c2 = do
  b <- dist bern 0.5
  dist (b  `condition` bern) 0.4
  return b
-}

biased_coin22 =
  momenta . drop 1000 $
   mcmC 10000 (biased_coin True False)
-- beta 2 2, av = 0.5, sigma2 = 5e-2
-- original Metropolis
-- (0.49433040918945736,5.013851472980666e-2)
-- incremental
-- (0.49910027237934973,4.97358737659099e-2)

{-
test_multiple_conditions13 =
  momenta . drop 1000 $
   mcmC 10000 (prog_multiple_conditions False False)
-- beta 1 3: 0.25, sigma2 = 3.75e-2
-- original Metropolis
-- (0.25144728141545775,3.782788408489756e-2)
-- incremental
-- (0.2445212550806874,3.651432172401049e-2)
-}

-- Example by Yufei Cai
-- See also https://github.com/p2t2/figaro/issues/548

pcorr pfuzz = do
  x <- dist bern 0.5
  y <- dist bern 0.5
  dist (True `condition` fuzzy2 pfuzz xor) x y
  return y
 where
   -- Creating a custom distribution
   fuzzy2 pfuzz op x y = let v = op x y in
     categorical [(v,pfuzz),(not v,1-pfuzz)]
   xor y x = not (x == y)

_ = hist $ mcmC 1000 (pcorr 0.1)
-- fromList [(False,419),(True,581)]

-- Branching and conditioning

post_bias c = do
  coin <- dist bern 0.5
  if_ coin (dist (c `condition` normal) 0 1)
           (dist (c `condition` normal) 100 1)
  return coin

_ = mcmC 10 (post_bias 1)
-- All true

pcond4 = do
  x <- dist categorical (Val [(1,0.5), (2,0.5)])
  if_ ((==1) <$> x)
     (return x)
     (do
       dist (True `condition` bern) 0.5
       dist categorical (Val [(20,0.5), (21,0.5)]))

_ = hist $ mcmC 1000 pcond4
{-
*Examples1> hist $ mcmC 1000 pcond4
fromList [(1,648),(20,174),(21,178)]
*Examples1> hist $ mcmC 10000 pcond4
fromList [(1,6697),(20,1641),(21,1662)]
-}
-- Exact:
--      [(0.166666666666666657, V 21); (0.166666666666666657, V 20);
--       (0.66666666666666663, V 1)])

-- The grass model
{-
grass = do
  rain        <- dist bern 0.3
  sprinkler   <- dist bern 0.5
  let obs = dist (True `condition` bern)
  if_ rain
    (obs 0.9)
    (if_ sprinkler
        (obs 0.8)
        (obs 0.1))
  return rain
-}


grass = do
  rain        <- dist bern 0.3
  sprinkler   <- dist bern 0.5
  --  dist (nor 0.9 0.8 0.1) rain sprinkler
  grass_is_wet <- dist (True `condition` nor 0.9 0.8 0.1) rain sprinkler
  return rain

-- noisy-or function
nor strengthX strengthY noise x y =
  bern $ 1 - nnot (1-strengthX) x * nnot (1-strengthY) y * (1-noise)
  
-- noisy not function
nnot :: Num a => a -> Bool -> a
nnot p True  = p
nnot p False = 1

grass_run = let ns = 20000
                r  = mcmC ns grass
            in
             ((fromIntegral . length . filter id $ r) /
              (fromIntegral ns :: Double))
-- 0.46815 for 20000
-- 0.4684714427203697  -- theoretical

{-
-- The simplest program (See Fig 1 of Hur et al, FSTTCS15)

start_example = do
  x <- bernN (Val 0.5)
  y <- bernN (Val 0.5)
  observe (x || y)
  return (x,y)
-- (false,false) is 0; teh remainders 1/3

-- Problematic (for Wingate et al) example (See Fig 2 of Hur et al, FSTTCS15)

-}

-- Example to illustrate MH algorithm

mhex = do
  x <- dist normal 0 1
  y <- dist normal x 2
  if_ ((>0.5) <$> x)
        (return x)
        (do {z <- dist beta 1 1; return (y + z)})
