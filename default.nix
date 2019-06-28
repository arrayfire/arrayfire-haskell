{ pkgs ? import <nixpkgs> {}
}:
with pkgs.haskell.lib;
let
  af = pkgs.callPackage ./arrayfire.nix {};
  fire = pkgs.haskellPackages.callCabal2nix "arrayfire" ./. { inherit af; };
in
  enableCabalFlag fire "disable-default-paths"
