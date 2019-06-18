{ pkgs ? import <nixpkgs> {} }:
let
  pkg = (import ./default.nix {}).env;
in
  pkgs.lib.overrideDerivation pkg (drv: {
    shellHook = ''
      function ghcid () {
        ${pkgs.haskellPackages.ghcid}/bin/ghcid -c 'cabal repl'
      };
      function test-runner () {
         ${pkgs.ag}/bin/ag -l | \
           ${pkgs.entr}/bin/entr sh -c \
             'cabal build test && dist/build/test/test +RTS -N -RTS'
      }
    '';
  })
