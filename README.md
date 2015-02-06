# github-haskell

A well tested, GitHub API client using `wreq` as a backend

The project uses Stackage to maintain build stability

To install:
```
cabal sandbox init
cabal install --only-dep --enable-test -jN
```

where N = \<the number of cores in your machine\>

To build:

```
cabal build
```

Then run the test suite:

```
cabal test
```
