# A collection of useful functions

A collection of useful functions are contained inside this repo so it can be used readily in new projects and by other developers. Tasty is used to do the testing. There may be better ways to write these functions but they are tested and they work well on-chain.

## Build and Test

To build the project from the parent directory run these cabal commands.

```bash
cd useful-funcs
cabal clean
cabal update
cabal build -w ghc-8.10.7
```

To run all the tests from inside the useful-funcs contract folder run `cabal test`.

```bash
cd useful-funcs
cabal test
```

## Using the Repo

Add this to the cabal.project file and update the tag to the most current release.

```cabal
source-repository-package
  type: git
  location: https://github.com/logicalmechanism/useful-funcs
  tag: b86b01fd0baf5e9d79b18272f7236a991991b225
  subdir: useful-funcs
```

Then add in `useful-funcs` into your .cabal file as a build dependency.

```cabal
  ...
  exposed-modules: Contract

  build-depends:   useful-funcs
  ...
```

Inside the contract, useful functions may be imported with the code below.

```hs
import UsefulFuncs
```

Now the contract has access to all the useful functions inside the repo.

## Documentation

[Documentation Link](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicalmechanism/useful-funcs/main/docs/UsefulFuncs.html)

To compile and view the haddock documentation run these commands from the parent directory.

```bash
cd useful-funcs
cabal haddock --ghc --cabal-file=useful-funcs.cabal >> output
docPath=$(tail -n 1 output)
rm -rf ../docs
cp -r ${docPath%index.html} ../docs
cd ../docs
firefox index.html
```

To compile and view the haddock documentation immediately run these commands from the parent directory.

```bash
cd useful-funcs
cabal haddock --ghc --cabal-file=useful-funcs.cabal >> output
firefox $(tail -n 1 output)
```

Replace firefox with any suitable browser if requried.