{ pkgs ? import <nixpkgs> { config.allowUnfree = true; } }:
# Latest arrayfire is not yet procured w/ nix.
let
  pkg = pkgs.haskellPackages.callCabal2nix "arrayfire" ./. {
    af = null;
    quickcheck-classes = pkgs.haskellPackages.quickcheck-classes_0_6_4_0;
  };
in
  pkg
