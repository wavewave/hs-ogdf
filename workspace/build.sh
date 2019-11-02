ghc gen/Gen.hs &&  \
./gen/Gen &&  \
cabal install OGDF &&  \
cabal exec -- ghc example/hierarchy-hs.hs && \
cabal exec -- ghc example/manual-hs.hs && \
cabal exec -- ghc example/manual2.hs
