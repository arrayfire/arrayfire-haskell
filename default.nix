{ pkgs ? import <nixpkgs> { config.allowUnfree = true; } }:
# Latest arrayfire is not yet procured w/ nix.
let
  pkg = pkgs.haskellPackages.callCabal2nix "arrayfire" ./. {
    af = null;
  };
in
  pkg
