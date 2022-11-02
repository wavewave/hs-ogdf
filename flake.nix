{
  description = "OGDF";
  inputs = {
    # nixpkgs/master on 2022-10-21
    nixpkgs.url =
      "github:NixOS/nixpkgs/71c5816834f93840dd301ec384c9d7947e97c27d";
    flake-utils.url = "github:numtide/flake-utils";
    fficxx = {
      url = "github:wavewave/fficxx/master";
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

        hpkgsFor = compiler:
          pkgs.haskell.packages.${compiler}.extend (hself: hsuper:
            (fficxx.haskellOverlay.${system} pkgs hself hsuper
              // haskellOverlay pkgs hself hsuper));

        mkPackages = compiler: { inherit (hpkgsFor compiler) ogdf OGDF; };

        # TODO: use haskell.packages.(ghc).shellFor
        mkShellFor = compiler:
          let
            hsenv = (hpkgsFor compiler).ghcWithPackages (p: [
              p.cabal-install
              p.extra
              p.fficxx
              p.fficxx-runtime
              p.stdcxx
              p.monad-loops
              p.dotgen
            ]);
          in pkgs.mkShell {
            buildInputs = [
              hsenv
              (hpkgsFor compiler).ogdf
              pkgs.pkgconfig
              pkgs.nixfmt
              pkgs.ormolu
              pkgs.graphviz
            ];
            shellHook = "";
          };

        supportedCompilers = [ "ghc902" "ghc924" "ghc942" ];
      in {
        packages =
          pkgs.lib.genAttrs supportedCompilers (compiler: hpkgsFor compiler);

        inherit haskellOverlay;

        devShells =
          pkgs.lib.genAttrs supportedCompilers (compiler: mkShellFor compiler);
      });
}
