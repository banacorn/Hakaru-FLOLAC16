{-# LANGUAGE RankNTypes #-}

module Distribution where

import System.Random

type LogLikelihood = Double
data Dist a = Dist {logDensity :: a -> LogLikelihood,
                    distSample :: forall g. RandomGen g => g ->
                                  (a, LogLikelihood, g)}

minusInf = log 0

mapFst :: (t -> s) -> (t, u) -> (s, u)
mapFst f (a,b) = (f a, b)

dirac :: (Eq a) => a -> Dist a
dirac theta =
  Dist {logDensity = (\x -> if x == theta then 0 else minusInf),
        distSample = (\g -> (theta,0,g))}

bern :: Double -> Dist Bool
bern p = Dist {logDensity = uns,
               distSample = \g ->
                              let (t, g') = randomR (0, 1) g
                                  x = t <= p
                              in (x, uns x, g')}
 where
   uns x = if x then lT else lF
   lT = log p
   lF = log (1-p)


-- Uniformly from a given set
uniformly :: Eq a => [a] -> Dist a
uniformly [] = error "uniformly: empty list"
uniformly lst = 
  Dist {logDensity = \x -> if x `elem` lst then ll else minusInf,
        distSample = \g -> let (i,g1) = randomR (0,len-1) g in
                           (lst !! i, ll, g1)}
 where ll = - log (fromIntegral len)
       len = length lst

uniform :: Double -> Double -> Dist Double
uniform lo hi =
  Dist {logDensity = uniformLogDensity,
        distSample = \g -> case randomR (lo, hi) g of
                              (x,g1) -> (x, l, g1)}
 where
   uniformLogDensity x | lo <= x && x <= hi = l
   uniformLogDensity _ = minusInf
   l = log (recip (hi - lo))

marsaglia :: (RandomGen g, Random a, Ord a, Floating a) => g -> ((a, a), g)
marsaglia g0 = -- "Marsaglia polar method"
  let (x, g1) = randomR (-1,1) g0
      (y, g ) = randomR (-1,1) g1
      s       = x * x + y * y
      q       = sqrt ((-2) * log s / s)
  in if 1 >= s && s > 0 then ((x * q, y * q), g) else marsaglia g

{-
choose :: (RandomGen g) => Mixture k -> g -> (k, Prob, g)
choose (Mixture m) g0 =
  let peak = maximum (M.elems m)
      unMix = M.map (LF.fromLogFloat . (/peak)) m
      total = M.foldl' (+) (0::Double) unMix
      (p, g) = randomR (0, total) g0
      f !k !v b !p0 = let p1 = p0 + v in if p <= p1 then k else b p1
      err p0 = error ("choose: failure p0=" ++ show p0 ++
                      " total=" ++ show total ++
                      " size=" ++ show (M.size m))
  in (M.foldrWithKey f err unMix 0, LF.logFloat total * peak, g)

chooseIndex :: (RandomGen g) => [Double] -> g -> (Int, g)
chooseIndex probs g0 =
  let (p, g) = random g0
      k = fromMaybe (error ("chooseIndex: failure p=" ++ show p))
                    (findIndex (p <=) (scanl1 (+) probs))
  in (k, g)
-}

{-# INLINE normal_rng #-}
normal_rng :: (Real a, Floating a, Random a, RandomGen g) =>
              a -> a -> g -> (a, g)
normal_rng mu sd g | sd > 0 = case marsaglia g of
                                ((x, _), g1) -> (mu + sd * x, g1)
normal_rng _ _ _ = error "normal: invalid parameters"

normalLogDensity :: Floating a => a -> a -> a -> a
normalLogDensity mu sd x = (-tau * square (x - mu)
                            + log (tau / pi / 2)) / 2
  where square y = y * y
        tau = 1 / square sd

normal :: Double -> Double -> Dist Double 
normal mu sd =
  let uns = normalLogDensity mu sd in
  Dist {logDensity = uns,
        distSample = \g -> case normal_rng mu sd g of
                            (x,g1) -> (x, uns x, g1)}

-- This is unnormalized (that is, up to a constant)
-- It doesn't matter since we are always interested in changes in LL
categoricalLogDensity :: Eq b => [(b, Double)] -> b -> Double
categoricalLogDensity lst x = maybe minusInf log $ (lookup x lst)

categoricalSample lst g = loop p lst
  where
    total = sum $ map snd lst
    (p, g1) = randomR (0, total) g
    loop p ((x,p0):t) | p < p0 = (x,log p0,g1)
                      | otherwise = loop (p-p0) t

categorical :: Eq a => [(a,Double)] -> Dist a
categorical lst = Dist {logDensity  = categoricalLogDensity lst,
                         distSample = categoricalSample lst}

{-
lnFact :: Integer -> Double
lnFact = logFactorial

-- Makes use of Atkinson's algorithm as described in:
-- Monte Carlo Statistical Methods pg. 55
--
-- Further discussion at:
-- http://www.johndcook.com/blog/2010/06/14/generating-poisson-random-values/
poisson_rng :: (RandomGen g) => Double -> g -> (Integer, g)
poisson_rng lambda g0 = make_poisson g0
   where smu = sqrt lambda
         b  = 0.931 + 2.53*smu
         a  = -0.059 + 0.02483*b
         vr = 0.9277 - 3.6224/(b - 2)
         arep  = 1.1239 + 1.1368/(b-3.4)
         lnlam = log lambda

         make_poisson :: (RandomGen g) => g -> (Integer,g)
         make_poisson g = let (u, g1) = randomR (-0.5,0.5) g
                              (v, g2) = randomR (0,1) g1
                              us = 0.5 - abs u
                              k = floor $ (2*a / us + b)*u + lambda + 0.43 in
                          case () of
                            () | us >= 0.07 && v <= vr -> (k, g2)
                            () | k < 0 -> make_poisson g2
                            () | us <= 0.013 && v > us -> make_poisson g2
                            () | accept_region us v k -> (k, g2)
                            _  -> make_poisson g2

         accept_region :: Double -> Double -> Integer -> Bool
         accept_region us v k = log (v * arep / (a/(us*us)+b)) <=
                                -lambda + (fromIntegral k)*lnlam - lnFact k

poisson :: Double -> Dist Integer
poisson l =
    let poissonLogDensity l' x | l' > 0 && x> 0 = (fromIntegral x)*(log l') - lnFact x - l'
        poissonLogDensity l' x | x==0 = -l'
        poissonLogDensity _ _ = log 0
    in Dist {logDensity = poissonLogDensity l . fromDiscrete,
             distSample = mapFst Discrete . poisson_rng l}
-}

-- Adapted from http://people.sc.fsu.edu/~burkardt/f_src/asa245/asa245.html

-- | Compute the logarithm of the gamma function Γ(/x/).  Uses
-- Algorithm AS 245 by Macleod.
--
-- Gives an accuracy of 10-12 significant decimal digits, except
-- for small regions around /x/ = 1 and /x/ = 2, where the function
-- goes to zero.  For greater accuracy, use 'logGammaL'.
--
-- Returns ∞ if the input is outside of the range (0 < /x/ ≤ 1e305).
-- | Positive infinity.
m_pos_inf :: Double
m_pos_inf = 1/0
{-# INLINE m_pos_inf #-}

logGamma :: Double -> Double
logGamma x
    | x <= 0    = m_pos_inf
    -- Handle positive infinity. logGamma overflows before 1e308 so
    -- it's safe
    | x > 1e308 = m_pos_inf
    -- Normal cases
    | x < 1.5   = a + c *
                  ((((r1_4 * b + r1_3) * b + r1_2) * b + r1_1) * b + r1_0) /
                  ((((b + r1_8) * b + r1_7) * b + r1_6) * b + r1_5)
    | x < 4     = (x - 2) *
                  ((((r2_4 * x + r2_3) * x + r2_2) * x + r2_1) * x + r2_0) /
                  ((((x + r2_8) * x + r2_7) * x + r2_6) * x + r2_5)
    | x < 12    = ((((r3_4 * x + r3_3) * x + r3_2) * x + r3_1) * x + r3_0) /
                  ((((x + r3_8) * x + r3_7) * x + r3_6) * x + r3_5)
    | x > 3e6   = k
    | otherwise = k + x1 *
                  ((r4_2 * x2 + r4_1) * x2 + r4_0) /
                  ((x2 + r4_4) * x2 + r4_3)
  where
    (a , b , c)
        | x < 0.5   = (-y , x + 1 , x)
        | otherwise = (0  , x     , x - 1)

    y      = log x
    k      = x * (y-1) - 0.5 * y + alr2pi
    alr2pi = 0.918938533204673

    x1 = 1 / x
    x2 = x1 * x1

    r1_0 =  -2.66685511495;   r1_1 =  -24.4387534237;    r1_2 = -21.9698958928
    r1_3 =  11.1667541262;    r1_4 =    3.13060547623;   r1_5 =   0.607771387771
    r1_6 =  11.9400905721;    r1_7 =   31.4690115749;    r1_8 =  15.2346874070

    r2_0 = -78.3359299449;    r2_1 = -142.046296688;     r2_2 = 137.519416416
    r2_3 =  78.6994924154;    r2_4 =    4.16438922228;   r2_5 =  47.0668766060
    r2_6 = 313.399215894;     r2_7 =  263.505074721;     r2_8 =  43.3400022514

    r3_0 =  -2.12159572323e5; r3_1 =    2.30661510616e5; r3_2 =   2.74647644705e4
    r3_3 =  -4.02621119975e4; r3_4 =   -2.29660729780e3; r3_5 =  -1.16328495004e5
    r3_6 =  -1.46025937511e5; r3_7 =   -2.42357409629e4; r3_8 =  -5.70691009324e2

    r4_0 = 0.279195317918525;  r4_1 = 0.4917317610505968;
    r4_2 = 0.0692910599291889; r4_3 = 3.350343815022304
    r4_4 = 6.012459259764103

-- Direct implementation of  "A Simple Method for Generating Gamma Variables"
-- by George Marsaglia and Wai Wan Tsang.
gamma_rng :: (RandomGen g) => Double -> Double -> g -> (Double, g)
gamma_rng shape _   _ | shape <= 0.0  = error "gamma: got a negative shape paramater"
gamma_rng _     scl _ | scl <= 0.0  = error "gamma: got a negative scale paramater"
gamma_rng shape scl g | shape <  1.0  = (gvar2, g2)
                      where (gvar1, g1) = gamma_rng (shape + 1) scl g
                            (w,  g2) = randomR (0,1) g1
                            gvar2 = scl * gvar1 * (w ** recip shape) 
gamma_rng shape scl g = 
    let d = shape - 1/3
        c = recip $ sqrt $ 9*d
        -- Algorithm recommends inlining normal generator
        n = normal_rng 1 c
        (v, g2) = until (\y -> fst y > 0.0) (\ (_, g') -> normal_rng 1 c g') (n g)
        x = (v - 1) / c
        sqr = x * x
        v3 = v * v * v
        (u, g3) = randomR (0.0, 1.0) g2
        accept  = u < 1.0 - 0.0331*(sqr*sqr) || log u < 0.5*sqr + d*(1.0 - v3 + log v3)
    in case accept of
         True -> (scl*d*v3, g3)
         False -> gamma_rng shape scl g3

gammaLogDensity :: Double -> Double -> Double -> Double
gammaLogDensity shape scl | shape > 0 && scl > 0 =
  let logShape =  log shape
      logGShape = logGamma shape in
  \x -> if x < 0 then minusInf else
          scl * logShape - scl * x + (shape - 1) * log x - logGShape
gammaLogDensity _ _ = \_ -> minusInf

gamma :: Double -> Double -> Dist Double
gamma shape scl =
  let uns  = gammaLogDensity shape scl in
  Dist {logDensity = uns,
        distSample = \g -> let (x,g1) = gamma_rng shape scl g in (x,uns x, g1)}

beta_rng :: (RandomGen g) => Double -> Double -> g -> (Double, g)
beta_rng a b g | a <= 1.0 && b <= 1.0 =
                 let (u, g1) = randomR (0.0, 1.0) g
                     (v, g2) = randomR (0.0, 1.0) g1
                     x = u ** (recip a)
                     y = v ** (recip b)
                 in  case (x+y) <= 1.0 of
                       True -> (x / (x + y), g2)
                       False -> beta_rng a b g2
beta_rng a b g = let (ga, g1) = gamma_rng a 1 g
                     (gb, g2) = gamma_rng b 1 g1
                 in (ga / (ga + gb), g2)

betaLogDensity :: Double -> Double -> Double -> Double
betaLogDensity a b | a <= 0 || b <= 0 =
  error "beta: parameters must be positve" 
betaLogDensity a b =
  let factor = logGamma (a + b)
               - logGamma a
               - logGamma b
  in \x -> if x < 0 || x > 1 then error "beta: value must be between 0 and 1"
           else factor + (a - 1) * log x
                       + (b - 1) * log (1 - x)

beta :: Double -> Double -> Dist Double
beta a b = Dist {logDensity =  bab,
                 distSample = \g -> let (x,g1) = beta_rng a b g in
                                    (x, bab x, g1)}
 where bab = betaLogDensity a b

{-
laplace_rng :: (RandomGen g) => Double -> Double -> g -> (Double, g)
laplace_rng mu sd g = sample (randomR (0.0, 1.0) g)
   where sample (u, g1) = case u < 0.5 of
                            True  -> (mu + sd * log (u + u), g1)
                            False -> (mu - sd * log (2.0 - u - u), g1)

laplaceLogDensity :: Floating a => a -> a -> a -> a
laplaceLogDensity mu sd x = - log (2 * sd) - abs (x - mu) / sd

laplace :: Double -> Double -> Dist Double
laplace mu sd = Dist {logDensity = laplaceLogDensity mu sd . fromLebesgue,
                      distSample = mapFst Lebesgue . laplace_rng mu sd}

-- Consider having dirichlet return Vector
-- Note: This is acutally symmetric dirichlet
dirichlet_rng :: (RandomGen g) => Int ->  Double -> g -> ([Double], g)
dirichlet_rng n' a g' = normalize (gammas g' n')
  where gammas g 0 = ([], 0, g)
        gammas g n = let (xs, total, g1) = gammas g (n-1)
                         ( x, g2) = gamma_rng a 1 g1 
                     in ((x : xs), x+total, g2)
        normalize (b, total, h) = (map (/ total) b, h)

dirichletLogDensity :: [Double] -> [Double] -> Double
dirichletLogDensity a x | all (> 0) x = sum' (zipWith logTerm a x) + logGamma (sum a)
  where sum' = foldl' (+) 0
        logTerm b y = (b-1) * log y - logGamma b
dirichletLogDensity _ _ = error "dirichlet: all values must be between 0 and 1"
-}
