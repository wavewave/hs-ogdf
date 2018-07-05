{ stdenv, haskellPackages }:

let
  OGDF-src = import ./gen.nix { inherit stdenv haskellPackages; };
in

{ mkDerivation, base, fficxx, fficxx-runtime, stdenv, template-haskell, stdcxx, ogdf }:
mkDerivation {
  pname = "OGDF";
  version = "0.0";
  src = OGDF-src;
  libraryHaskellDepends = [
    base fficxx fficxx-runtime template-haskell stdcxx
  ];
  librarySystemDepends = [ ogdf ];
  license = stdenv.lib.licenses.bsd3;
}
