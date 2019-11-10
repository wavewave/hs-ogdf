{ pkgs ? import <nixpkgs> {}
, fficxxSrc ?
    pkgs.fetchgit {
      url = "git://github.com/wavewave/fficxx.git";
      rev = "69a2eb906c2ef5c56ba2f79558ee83e91206bde4";
      sha256 = "0dipx37gc4rfx48n7xn1fjmq8q9zh2cy4vs4ha7i8cgqzls88552";
    }
}:

with pkgs;

let
  # TODO: should be packaged into the upstream nixpkgs.
  ogdf = callPackage ./ogdf/default.nix {};

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
    }
    // callPackage ./default.nix { inherit fficxxSrc ogdf; } self super;
  };

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
    OGDF
    monad-loops
  ]);

in

stdenv.mkDerivation {
  name = "OGDF-env";

  buildInputs = [
    hsenv
  ];

}
