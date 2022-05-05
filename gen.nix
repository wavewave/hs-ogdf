{ pkgs }:

with pkgs;

let

  hsenv =
    haskellPackages.ghcWithPackages (p: with p; [ fficxx fficxx-runtime ]);

in stdenv.mkDerivation {
  name = "OGDF-src";
  buildInputs = [ hsenv ];
  src = ./.;
  buildPhase = ''
    ghc Gen.hs
    ./Gen ./template
  '';
  installPhase = ''
    mkdir -p $out
    cp -a OGDF/* $out
  '';
}
