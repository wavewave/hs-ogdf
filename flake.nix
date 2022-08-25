{
  description = "OGDF";
  inputs = {
    # nixpkgs/master on 2022-07-18
    nixpkgs.url =
      "github:NixOS/nixpkgs/31997025a4d59f09a9b4c55a3c6ff5ade48de2d6";
    flake-utils.url = "github:numtide/flake-utils";
    fficxx = {
      url = "github:wavewave/fficxx/aarch64-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = { self, nixpkgs, flake-utils, fficxx }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlayGHC = final: prev: {
          haskellPackages = prev.haskellPackages;
        };
        pkgs = import nixpkgs {
          overlays = [ overlayGHC (fficxx.overlay.${system}) ];
          inherit system;
          config.allowBroken = true;
        };

        ogdf = pkgs.callPackage ./ogdf { };

        finalHaskellOverlay = self: super:
          (import ./default.nix { inherit pkgs ogdf; } self super);

        newHaskellPackages = pkgs.haskellPackages.extend finalHaskellOverlay;

      in {
        packages = {
          inherit ogdf;
          inherit (newHaskellPackages) OGDF;
        };

        # see these issues and discussions:
        # - https://github.com/NixOS/nixpkgs/issues/16394
        # - https://github.com/NixOS/nixpkgs/issues/25887
        # - https://github.com/NixOS/nixpkgs/issues/26561
        # - https://discourse.nixos.org/t/nix-haskell-development-2020/6170
        overlay = final: prev: {
          ogdf = final.callPackage ./ogdf { };
          haskellPackages = prev.haskellPackages.override (old: {
            overrides =
              final.lib.composeExtensions (old.overrides or (_: _: { }))
              finalHaskellOverlay;
          });
        };

        devShell = with pkgs;
          let
            hsenv = haskellPackages.ghcWithPackages (p: [
              p.cabal-install
              p.extra
              p.fficxx
              p.fficxx-runtime
              p.ormolu
              p.stdcxx
              p.formatting
              p.monad-loops
            ]);
          in mkShell {
            buildInputs = [ hsenv ogdf pkgconfig nixfmt ];
            shellHook = "";
          };
      });
}
