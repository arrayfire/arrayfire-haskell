{ pkgs ? import <nixpkgs> { config.allowUnfree = true; } }:
# Latest arrayfire is not yet procured w/ nix.
let
  af = pkgs.callPackage ./nix {};
  pkg = pkgs.haskellPackages.callCabal2nix "arrayfire" ./. { inherit af; };
in
  with pkgs.haskell.lib;
  enableCabalFlag pkg "disable-default-paths"
