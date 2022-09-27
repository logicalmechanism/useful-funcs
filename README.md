# A collection of useful functions

A collection of useful functions are contained inside this repo so it can be used readily in new projects and by other developers. Tasty is used to do the testing. There may be better ways to write these functions but they are tested and they work well on-chain.

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

Add this to the cabal.project file and update the tag to the most current release.

```cabal
source-repository-package
  type: git
  location: https://github.com/logicalmechanism/useful-funcs
  tag: 9f5f3287eff7d5c8a9d6a545f8b275db24376430
  subdir: useful-funcs
```

Then add in useful-funcs into your .cabal file for a build dependency.

```cabal
  ...
  exposed-modules: Contract
  build-depends:   useful-funcs
  ...
```

Inside the contract, useful functions can be imported with

```hs
import UsefulFuncs
```

Now the contract has access to all the useful functions inside the repo.