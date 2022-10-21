{
  description = "OGDF";
  inputs = {
    # nixpkgs/master on 2022-07-18
    nixpkgs.url =
      "github:NixOS/nixpkgs/31997025a4d59f09a9b4c55a3c6ff5ade48de2d6";
    flake-utils.url = "github:numtide/flake-utils";
    fficxx = {
      url = "github:wavewave/fficxx/no-intermediate-step";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = { self, nixpkgs, flake-utils, fficxx }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        haskellOverlay = final: hself: hsuper:
          (import ./default.nix { pkgs = final; } hself hsuper);

        hpkgsGhc902 = pkgs.haskell.packages.ghc902.extend (hself: hsuper:
          (fficxx.haskellOverlay.${system} pkgs hself hsuper
            // haskellOverlay pkgs hself hsuper));

      in {
        packages = { inherit (hpkgsGhc902) ogdf OGDF; };

        devShells.default = let
          hsenv = hpkgsGhc902.ghcWithPackages (p: [
            p.cabal-install
            p.extra
            p.fficxx
            p.fficxx-runtime
            p.stdcxx
            p.formatting
            p.monad-loops
          ]);
        in pkgs.mkShell {
          buildInputs =
            [ hsenv hpkgsGhc902.ogdf pkgs.pkgconfig pkgs.nixfmt pkgs.ormolu ];
          shellHook = "";
        };
      });
}
