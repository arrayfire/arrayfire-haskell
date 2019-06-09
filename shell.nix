{ pkgs ? import <nixpkgs> {} }:
let
  pkg = (import ./default.nix {}).env;
in
  pkgs.lib.overrideDerivation pkg (drv: {
    shellHook = ''
      function ghcid () {
        ${pkgs.haskellPackages.ghcid}/bin/ghcid -c 'cabal repl lib:arrayfire'
      };
    '';
  })
