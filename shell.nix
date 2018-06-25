{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  ogdf = callPackage ./default.nix {};

  fficxxSrc = fetchgit {
                url = "https://github.com/wavewave/fficxx";
                rev = "b04507caddd3492df34e0c9c44ffd676c973d4b2";
                sha256 = "0fxc9yi4h0kpm8kpak2256jhjkf53dml9mlcip3v5z3bak6idg7b";
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
