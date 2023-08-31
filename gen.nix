{stdenv}: hself: let
  hsenv = hself.ghcWithPackages (p: with p; [fficxx fficxx-runtime optparse-applicative]);
in
  stdenv.mkDerivation {
    name = "OGDF-src";
    buildInputs = [hsenv];
    src = ./.;
    buildPhase = ''
      ghc Gen.hs OGDFIDL.hs
      ./Gen gen -t ./template
    '';
    installPhase = ''
      mkdir -p $out
      cp -a OGDF/* $out
    '';
  }
