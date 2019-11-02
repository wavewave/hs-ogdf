{ stdenv, haskellPackages }:

let
  hsenv = haskellPackages.ghcWithPackages (p: with p; [ fficxx-runtime fficxx ]);
in

stdenv.mkDerivation {
  name = "OGDF-src";
  buildInputs = [ hsenv ];
  src = ./.;
  buildPhase = ''
    ghc Gen.hs
    ./Gen
  '';
  installPhase = ''
    mkdir -p $out
    cp -a OGDF/* $out
  '';
}
