ghc gen/Gen.hs
./gen/Gen
cabal install OGDF
cabal exec -- ghc example/hierarchy-hs.hs
