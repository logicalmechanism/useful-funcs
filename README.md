# A collection of useful functions

I put all my helper and useful functions inside this repo so it can be used readily in new projects and by other developers. I used tasty to do the testing on the functions. There may be better ways to write these functions but this is what I use in my projects. They are tested and they work well on-chain.

To build out the project.

```hs
cabal clean
cabal update
cabal build -w ghc-8.10.7
```

To run all the tests.

```hs
cabal test
```