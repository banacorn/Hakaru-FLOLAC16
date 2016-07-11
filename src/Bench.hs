module Main where

-- ghc -O2  -rtsopts Bench.hs
-- To run this code
-- GHCRTS="-tstderr" ./Bench 10 10000

import Syntax
import Util
import Control.Applicative

import System.Environment


phier n =
  (iterate (\m -> do {x<-m; dist normal x 3}) $ dist normal 0 1) !! n

main = do
  [n',nchain'] <- getArgs
  let n = read n' :: Int
  let nchain = read nchain' :: Integer
  print . momenta $ mcmC nchain (phier n)

{-
1000
(3.4117556376053577,32.27027981263249)
<<ghc: 12270984 bytes, 24 GCs, 56940/69568 avg/max bytes residency (2 samples), 1M in use, 0.00 INIT (0.00 elapsed), 0.03 MUT (0.03 elapsed), 0.00 GC (0.00 elapsed) :ghc>>

10 000
(-3.151396208304257,54.17557680794696)
<<ghc: 120791696 bytes, 235 GCs, 56940/69568 avg/max bytes residency (2 samples), 2M in use, 0.00 INIT (0.00 elapsed), 0.20 MUT (0.21 elapsed), 0.01 GC (0.01 elapsed) :ghc>>

100 000
(-0.7051036715134076,84.35841806407852)
<<ghc: 1205953056 bytes, 2346 GCs, 1104692/2575080 avg/max bytes residency (5 samples), 6M in use, 0.00 INIT (0.00 elapsed), 2.02 MUT (2.03 elapsed), 0.05 GC (0.05 elapsed) :ghc>>

(0.2246675771924041,93.50779949355687)
<<ghc: 12044581992 bytes, 23439 GCs, 8450544/25015768 avg/max bytes residency (9 samples), 51M in use, 0.00 INIT (0.00 elapsed), 19.84 MUT (19.91 elapsed), 0.56 GC (0.56 elapsed) :ghc>>
-}
