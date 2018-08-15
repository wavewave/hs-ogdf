{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  ogdf = callPackage ./default.nix {};

  fficxxSrc = fetchgit {
                url = "https://github.com/wavewave/fficxx";
                rev = "1f62491453a5567c92f01091c3831c50bc45facb";
                sha256 = "1mda4vr7qqyh7vsyl6vpa74fvk94cp43m8gmsivv952lwazr9mh6";
              };
  #fficxxSrc = ../fficxx; 

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
