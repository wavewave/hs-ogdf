{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  ogdf = callPackage ./default.nix {};

  fficxxSrc = fetchgit {
                url = "https://github.com/wavewave/fficxx";
                rev = "63a52aed0cc033927af3ad0b745ba647fe873292";
                sha256 = "1g150gchbg22134wx6qpi683drccxn80sb4a39lwya9bkldwilla";
              };

  newHaskellPackages = haskellPackages.override {
                         overrides = self: super: {
                           "fficxx" = self.callCabal2nix "fficxx" (fficxxSrc + "/fficxx") {};
                           "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
                         };
                       };

  stdcxxNix = import (fficxxSrc + "/stdcxx-gen/default.nix") {
              inherit (pkgs) stdenv;
              haskellPackages = newHaskellPackages;
           };

  newHaskellPackagesFinal = haskellPackages.override {
                              overrides = self: super: {
                                "fficxx" = self.callCabal2nix "fficxx" (fficxxSrc + "/fficxx") {};
                                "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
                                "stdcxx" = self.callPackage stdcxxNix { };
                              };
                            };

  hsenv = newHaskellPackagesFinal.ghcWithPackages (p: with p; [
            fficxx
            fficxx-runtime
            formatting
            monad-loops
            stdcxx
            cabal-install
          ]);
in

stdenv.mkDerivation {
  name = "hs-ogdf-dev";
  buildInputs = [ ogdf hsenv ];
}
