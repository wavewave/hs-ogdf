rm -rf dist-newstyle
cd ..; ghc Gen.hs ; cd workspace
../Gen
cabal new-build hs-ogdf
