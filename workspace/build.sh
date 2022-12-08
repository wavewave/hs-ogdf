rm -rf dist-newstyle
rm *.o
cabal build fficxx
cabal exec runhaskell ../../fficxx/stdcxx-gen/Gen.hs
cabal build stdcxx
cabal exec -- ghc ../Gen.hs ../OGDFIDL.hs -package optparse-applicative
../Gen gen -t ../template
cabal build OGDF
cabal exec -- ghc -package extra hierarchy-hs.hs
cabal exec -- ghc manual-hs.hs
cabal exec -- ghc manual2.hs -package monad-loops
cabal exec -- ghc manual3.hs -package monad-loops

c++ -std=c++11  -c  multilevel.cc
c++ -o multilevel.exe multilevel.o -logdf


# cabal exec -- ghc -i.. ../DepGraph.hs ../OGDFIDL.hs -package dotgen && ../DepGraph testX.dot && dot -Tsvg -o testX.svg testX.dot && open testX.svg
