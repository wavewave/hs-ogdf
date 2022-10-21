rm -rf dist-newstyle
runhaskell ../Gen.hs
cabal build OGDF
cabal exec -- ghc -package extra hierarchy-hs.hs
cabal exec -- ghc manual-hs.hs
cabal exec -- ghc manual2.hs -package monad-loops
cabal exec -- ghc manual3.hs -package monad-loops

c++ -std=c++11  -c  multilevel.cc
c++ -o multilevel.exe multilevel.o -logdf
