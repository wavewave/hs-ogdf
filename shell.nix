{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  ogdf = callPackage ./default.nix {};
in

stdenv.mkDerivation {
  name = "hs-ogdf-dev";
  buildInputs = [ ogdf ];
}