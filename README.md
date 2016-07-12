# Hakaru-FLOLAC16

* [埋込み確率プログラミング言語とその漸進評価](http://okmij.org/ftp/kakuritu/Hakaru10/PPL.pdf)
* [Problems of the Lightweight Implementation of Probabilistic Programming](http://okmij.org/ftp/kakuritu/Hakaru10/PPS2016.pdf)

# How to run this

1. Clone this repo (or just download it anyway)

    ```shell
    git clone git@github.com:banacorn/Hakaru-FLOLAC16.git
    cd Hakaru-FLOLAC16
    ```

2. Enter GHCi

    ```shell
    ghci Discrete1.hs
    ```

    For example, you can play with those examples with `runSample :: Int -> Dst a -> [a]` in `Discrete`

    ```
    *Discrete> runSample 10 twocoins
    [True,False,True,False,False,True,False,False,False,True]
    ```

3. You may need some of these packages: `mtl`, `transformers`, `random`

    ```shell
    cabal install mtl transformers random
    ```
