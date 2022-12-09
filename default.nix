{ pkgs }:

hself: hsuper:

let

  OGDF-src = (pkgs.callPackage ./gen.nix { }) hself;

in rec {
  # Haskell binding library
  "OGDF" = hself.callPackage ({ mkDerivation, base, fficxx, fficxx-runtime
    , stdenv, template-haskell, stdcxx, ogdf }:
    mkDerivation {
      pname = "OGDF";
      version = "0.0";
      src = OGDF-src;
      libraryHaskellDepends =
        [ base fficxx fficxx-runtime template-haskell stdcxx ];
      librarySystemDepends = [ ogdf ];
      license = pkgs.lib.licenses.bsd3;
    }) { ogdf = pkgs.ogdf; };
}
