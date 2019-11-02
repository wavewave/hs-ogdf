{ pkgs ? import <nixpkgs> {}, fficxxSrc }:

with pkgs;

let

  newHaskellPackages0 = haskellPackages.override {
    overrides = self: super: {
      "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
      "fficxx"         = self.callCabal2nix "fficxx"         (fficxxSrc + "/fficxx")         {};
    };
  };

  stdcxxNix = import (fficxxSrc + "/stdcxx-gen/default.nix") {
    inherit stdenv;
    haskellPackages = newHaskellPackages0;
  };

  newHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
      "fficxx"         = self.callCabal2nix "fficxx"         (fficxxSrc + "/fficxx")         {};
      "stdcxx"         = self.callPackage stdcxxNix {};
    };
  };

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
    fficxx
    fficxx-runtime
  ]);

in

stdenv.mkDerivation {
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
