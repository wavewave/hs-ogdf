{
  description = "OGDF";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    fficxx = {
      url = "github:wavewave/fficxx/master";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    fficxx,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        config.allowBroken = true;
      };

      haskellOverlay = final: hself: hsuper: (import ./default.nix {pkgs = final;} hself hsuper);

      hpkgsFor = compiler:
        pkgs.haskell.packages.${compiler}.extend (hself: hsuper: (fficxx.haskellOverlay.${system} pkgs hself hsuper
          // {
            "ormolu" =
              pkgs.haskell.lib.overrideCabal hsuper.ormolu
              (drv: {enableSeparateBinOutput = false;});
          }
          // haskellOverlay pkgs hself hsuper));

      mkPackages = compiler: {inherit (hpkgsFor compiler) OGDF;};

      # TODO: use haskell.packages.(ghc).shellFor
      mkShellFor = isEnv: compiler: let
        hsenv = (hpkgsFor compiler).ghcWithPackages (p:
          [
            p.extra
            p.fficxx
            p.fficxx-runtime
            p.optparse-applicative
            p.stdcxx
            p.monad-loops
            p.dotgen
          ]
          ++ (
            if isEnv
            then [p.OGDF]
            else []
          ));
        pyenv =
          pkgs.python3.withPackages
          (p: [p.sphinx p.sphinx_rtd_theme p.myst-parser]);
        prompt =
          if isEnv
          then "hs-ogdf-env"
          else "hs-ogdf-dev";
      in
        pkgs.mkShell {
          buildInputs = [
            hsenv
            pyenv
            pkgs.ogdf
            pkgs.cabal-install
            pkgs.pkgconfig
            pkgs.alejandra
            pkgs.ormolu
            pkgs.graphviz
          ];
          shellHook = ''
            export PS1="\n[${prompt}:\w]$ \0"
          '';
        };

      supportedCompilers = ["ghc962"];
      defaultCompiler = "ghc962";
    in rec {
      packages =
        pkgs.lib.genAttrs supportedCompilers (compiler: hpkgsFor compiler);

      inherit haskellOverlay;

      devShells =
        pkgs.lib.genAttrs supportedCompilers (compiler: mkShellFor false compiler)
        // {
          default = devShells.${defaultCompiler};
          env = mkShellFor true defaultCompiler;
        };
    });
}
