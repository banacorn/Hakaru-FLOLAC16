# Hakaru-FLOLAC16

* [埋込み確率プログラミング言語とその漸進評価](http://okmij.org/ftp/kakuritu/Hakaru10/PPL.pdf)
* [Problems of the Lightweight Implementation of Probabilistic Programming](http://okmij.org/ftp/kakuritu/Hakaru10/PPS2016.pdf)

# How to run this

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
