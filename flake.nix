{
  description = "OGDF";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    flake-utils.url = "github:numtide/flake-utils";
    fficxx = {
      url = "github:wavewave/fficxx/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, flake-utils, fficxx }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          overlays = [ fficxx.overlay ];
          inherit system;
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
            overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
              finalHaskellOverlay;
          });
        };

        devShell = with pkgs;
          let
            hsenv = haskellPackages.ghcWithPackages (p: [
              p.cabal-install
              p.fficxx
              p.fficxx-runtime
              p.stdcxx
              p.formatting
              p.monad-loops
            ]);
          in mkShell {
            buildInputs = [ hsenv ogdf pkgconfig ];
            shellHook = "";
          };
      }
  );
}
