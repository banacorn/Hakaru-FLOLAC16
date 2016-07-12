
-- Various utilities, to visualize distributions, etc.

module Util where

import qualified Data.Map as M
import System.IO
import Control.Monad

-- Very simple histogram, for a discrete distribution
hist :: Ord t => [t] -> M.Map t Int
hist = foldr (\e -> M.insertWith' (+) e 1) M.empty


-- compute the first two momenta
momenta :: [Double] -> (Double,Double)
momenta lst = (av,sigma2)
  where
    n   = fromIntegral $ length lst
    av  = sum lst / n
    sigma2 = (foldr (\x acc -> sqr (x - av) + acc) 0 lst) / (n - 1)
    sqr x = x * x

-- Tabulate the distribution
type Histogram = M.Map Double Double

-- The argument is the _hint_ to the number of bins
tabulate' :: Int -> [Double] -> Histogram
tabulate' nbins lst =
  foldr (\x -> M.insertWith' (+) (to_bin x) quantum) M.empty lst
 where
   mn = minimum lst
   mx = maximum lst
   quantum = 1 / fromIntegral (length lst)
   step_raw = if mn == mx then 1 else (mx - mn) / fromIntegral nbins
   step  = 10.0 ** (fromInteger . round $ log step_raw / log 10)
   to_bin x = fromInteger (floor (x / step)) * step

-- Tabulate with the given bin size
tabulate :: Double -> [Double] -> Histogram
tabulate step lst =
  foldr (\x -> M.insertWith' (+) (to_bin x) quantum) M.empty lst
 where
   quantum = 1 / fromIntegral (length lst)
   to_bin x = fromInteger (floor (x / step)) * step

-- Compute Kullback–Leibler divergence KL(p|q) of two distributions
-- given as histograms.
-- Here, p is supposed to be a `theoretical' distribution
--  Sum{ p i * log (p i / q i) }
kl :: Histogram -> Histogram -> Double
kl p q = M.foldlWithKey' f 0 p
 where f sum i pi | Just qi <- M.lookup i q = sum + pi * log (pi / qi)
                  | otherwise =
         error $ "Undefined KL divergence - q is 0 when p is not at point " ++
                 show i

plot :: Histogram -> IO ()
plot dat = do
  withFile fname WriteMode write_data
 where
   fname = "/tmp/plot.dat"
   write_data h = do
     hPutStrLn h "x  y"
     mapM_ (\ (k,v) -> hPutStrLn h $ unwords [show k,show v]) $ M.toList dat
