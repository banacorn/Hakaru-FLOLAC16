# Hakaru-FLOLAC16

* [埋込み確率プログラミング言語とその漸進評価](http://okmij.org/ftp/kakuritu/Hakaru10/PPL.pdf)
* [Problems of the Lightweight Implementation of Probabilistic Programming](http://okmij.org/ftp/kakuritu/Hakaru10/PPS2016.pdf)

# How to run this

1. Try this first
  
  1. Clone this repo (or just download it anyway)
  
    ```shell
    git clone git@github.com:banacorn/Hakaru-FLOLAC16.git
    cd Hakaru-FLOLAC16
    ```
    
  2. Build & Run
    
    ```shell
    cd src 
    ghc -O2  -rtsopts Bench.hs
    GHCRTS="-tstderr" ./Bench 10 10000
    ```
  
  3. Install whatever `ghc` complains, you may need some of these packages
  
    ```shell
    cabal install mtl transformers random
    ```
  
2. If it didn't work out

  1. [Install stack](http://docs.haskellstack.org/en/stable/README/#how-to-install)
  2. Clone this repo
  
    ```shell
    git clone git@github.com:banacorn/Hakaru-FLOLAC16.git
    cd Hakaru-FLOLAC16
    ```
  
  3. Build
  
    ```shell
    stack setup
    stack build
    ```
  
  4. Run
    
    ```
    stack exec hakaru-exe 10 100 -- +RTS -tstderr
    ```

Courtesy of Oleg Kiselyov
