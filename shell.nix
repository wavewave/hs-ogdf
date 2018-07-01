{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  ogdf = callPackage ./default.nix {};

  fficxxSrc = ../fficxx;

              #fetchgit {
              #  url = "https://github.com/wavewave/fficxx";
              #  rev = "9b151b3a0dc31fd1a4b4682dfbc8092143b97a86";
              #  sha256 = "0scwl1vyyf9j1ssy4zf85r988lyi3jsmf7aqrvjiacg2jz7194da";
              #};

  newHaskellPackages = haskellPackages.override {
                         overrides = self: super: {
                           "fficxx" = self.callCabal2nix "fficxx" (fficxxSrc + "/fficxx") {};
                           "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
                         };
                       };
  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            fficxx
            fficxx-runtime
            formatting
            monad-loops
            cabal-install
          ]);
in

stdenv.mkDerivation {
  name = "hs-ogdf-dev";
  buildInputs = [ ogdf hsenv ];
}
