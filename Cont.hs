-- Various simple examples of using Hakaru10

module ExTutorial where

import Syntax
import Util
import Control.Applicative

_ = plot $ tabulate 0.4 $ mcmC 10000 (dist normal 0 1)
_ = momenta $ mcmC 1000 (dist normal 0 1)

-- Warm-up: summing three normals (0,1) (1,2) (3,4)

n3 :: Model Double
n3 = undefined

_ = plot $ tabulate 0.4 $ mcmC 10000 n3
_ = momenta $ mcmC 1000 n3


-- Grass model
grass = do
  rain        <- dist bern 0.3
  sprinkler   <- dist bern 0.5
  --  dist (nor 0.9 0.8 0.1) rain sprinkler
  grass_is_wet <- dist (True `condition` nor 0.9 0.8 0.1) rain sprinkler
  return rain

-- noisy-or function
nor :: Double -> Double -> Double -> Bool -> Bool -> Distribution Bool
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

-- Appreciating conditioning
post_bias c = do
  coin <- dist bern 0.5
  if_ coin (dist (c `condition` normal) 0 1)
           (dist (c `condition` normal) 10 1)
  return coin

_ = mcmC 100 (post_bias 1)
-- All true
-- Then switch sigma to 10...



-- coin bias estimation
biased_coin c1 c2 = do
  b <- dist beta 1 1
  dist (c1 `condition` bern) b
  dist (c2 `condition` bern) b
  return b

biased_coin22 =
  momenta . drop 1000 $
   mcmC 10000 (biased_coin True False)

-- generalize to N tosses

-- Linear regression
-- y = a*x + b

-- (why not to use the deterministic formulas?
-- Because we may want to know exact distributions of parameters, and
-- because there may be very many parameters, in millions.)


type Param = (Double,Double)

params :: Model Param
params = do
  a <- dist normal 1 1
  b <- dist normal 5 2
  return (pair a b)

genpoints :: Param -> Model (Double,Double)  -- xy
genpoints (a,b) = do
  x <- dist uniform (-1.0) 1.0
  noise_sd <- (1/) <$> dist gamma 0.5 0.5
  y <- dist normal (pure a*x + pure b) 0.1
  return (pair x y)

[param1] = mcmC 1 params
points = mcmC 100 $ genpoints param1

linalg :: [(Double,Double)] -> Model Param
linalg xy = do
  ab <- params
  let a = fst <$> ab
  let b = snd <$> ab
  noise_sd <- (1/) <$> dist gamma 0.5 0.5
  let make_cond (x,y) = dist (y `condition` normal) (a*pure x+b) 0.1
  mapM_ make_cond xy
  return (pair a b)

_ = let (aa,bb) = unzip . mcmC 10 $ linalg points in (momenta aa, momenta bb)
