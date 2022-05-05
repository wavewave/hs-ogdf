{
  description = "OGDF";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.03";
    fficxx = {
      url = "github:wavewave/fficxx/0.6";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, fficxx }:
    let
      pkgs = import nixpkgs {
        overlays = [ fficxx.overlay ];
        system = "x86_64-linux";
      };

      ogdf = pkgs.callPackage ./ogdf { };

      finalHaskellOverlay = self: super:
        (import ./default.nix { inherit pkgs ogdf; } self super);

      newHaskellPackages = pkgs.haskellPackages.extend finalHaskellOverlay;

    in {
      packages.x86_64-linux = {
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

      devShell.x86_64-linux = with pkgs;
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
    };
}
