rm -rf dist-newstyle
rm *.o
ghc ../Gen.hs ../OGDFIDL.hs
../Gen gen -t ../template
cabal build OGDF
cabal exec -- ghc -package extra hierarchy-hs.hs
cabal exec -- ghc manual-hs.hs
cabal exec -- ghc manual2.hs -package monad-loops -package text
cabal exec -- ghc manual3.hs -package monad-loops

c++ -std=c++11  -c  multilevel.cc
c++ -o multilevel.exe multilevel.o -logdf -lCOIN
