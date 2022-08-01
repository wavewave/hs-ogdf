rm -rf dist-newstyle
runhaskell ../Gen.hs
cabal new-build OGDF
#cabal new-exec -- ghc hierarchy-hs.hs
#cabal new-exec -- ghc manual-hs.hs
cabal new-exec -- ghc manual2.hs -package monad-loops -package formatting
