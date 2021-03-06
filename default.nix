{ pkgs ? import <nixpkgs> {}, fficxxSrc, ogdf }:

let

  OGDF-src = pkgs.callPackage ./gen.nix { inherit fficxxSrc; };

in

self: super:

{
  "OGDF" = self.callPackage (
    {mkDerivation, base, fficxx, fficxx-runtime, stdenv, template-haskell, stdcxx, ogdf }:
      mkDerivation {
        pname = "OGDF";
        version = "0.0";
        src = OGDF-src;
        libraryHaskellDepends = [
          base fficxx fficxx-runtime template-haskell stdcxx
        ];
        librarySystemDepends = [ ogdf ];
        license = stdenv.lib.licenses.bsd3;
      }) { inherit ogdf; };
}
