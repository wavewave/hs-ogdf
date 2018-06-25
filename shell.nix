{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  ogdf = callPackage ./default.nix {};

  fficxxSrc = fetchgit {
                url = "https://github.com/wavewave/fficxx";
                rev = "c072177f6f5fbbb914feb2ea6c182d3a5f8cbae1";
                #b04507caddd3492df34e0c9c44ffd676c973d4b2
                sha256 = "0qzxlp322pli0bhvgy0mqpj2rzw51jamar4y95v8s4w8jka9dz6n";
              };

  newHaskellPackages = haskellPackages.override {
                         overrides = self: super: {
                           "fficxx" = self.callCabal2nix "fficxx" (fficxxSrc + "/fficxx") {};
                         };
                       };
  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            fficxx
            fficxx-runtime
            cabal-install
          ]);
in

stdenv.mkDerivation {
  name = "hs-ogdf-dev";
  buildInputs = [ ogdf hsenv ];
}
