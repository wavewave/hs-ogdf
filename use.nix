{
  pkgs ? import <nixpkgs> {},
  fficxxSrc ? (import ./nix/pinned.nix {inherit pkgs;}).fficxxSrc,
}:
with pkgs; let
  # TODO: should be packaged into the upstream nixpkgs.
  ogdf = callPackage ./ogdf/default.nix {};

  newHaskellPackages0 = haskellPackages.override {
    overrides = self: super: {
      "fficxx-runtime" =
        self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
      "fficxx" = self.callCabal2nix "fficxx" (fficxxSrc + "/fficxx") {};
    };
  };

  stdcxxNix = import (fficxxSrc + "/stdcxx-gen/default.nix") {
    inherit stdenv;
    haskellPackages = newHaskellPackages0;
  };

  newHaskellPackages = haskellPackages.override {
    overrides = self: super:
      {
        "fficxx-runtime" =
          self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime")
          {};
        "fficxx" = self.callCabal2nix "fficxx" (fficxxSrc + "/fficxx") {};
        "stdcxx" = self.callPackage stdcxxNix {};
      }
      // callPackage ./default.nix {inherit fficxxSrc ogdf;} self super;
  };

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [OGDF monad-loops]);
in
  stdenv.mkDerivation {
    name = "OGDF-env";

    buildInputs = [hsenv];
  }
