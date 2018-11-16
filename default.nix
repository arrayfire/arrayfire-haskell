{ pkgs ? import <nixpkgs> {} }:
  pkgs.haskellPackages.callPackage ./pkg.nix {}
