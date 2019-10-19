{ pkgs ? import <nixpkgs> {} }:
# Latest arrayfire is not yet procured w/ nix.
pkgs.haskellPackages.callCabal2nix "arrayfire" ./. { af = null; }
