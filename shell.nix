{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  ogdf = callPackage ./default.nix {};

  fficxxSrc = fetchgit {
                url = "https://github.com/wavewave/fficxx";
                rev = "e5b6cdae7965bff00223104697f67f9e3fc82836";
                sha256 = "10gpam0hikxqfkxzg8w2ya27spmx9y9z7968s2x5rf554c459kgg";
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
