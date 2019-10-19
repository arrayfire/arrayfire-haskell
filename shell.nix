{ pkgs ? import <nixpkgs> {} }:
let
  pkg = (import ./default.nix {}).env;
in
  pkgs.lib.overrideDerivation pkg (drv: {
    shellHook = ''
      export PATH=$PATH:${pkgs.haskellPackages.doctest}/bin
      function ghcid () {
        ${pkgs.haskellPackages.ghcid.bin}/bin/ghcid -c 'cabal v1-repl lib:arrayfire'
      };
      function test-runner () {
         ${pkgs.ag}/bin/ag -l | \
           ${pkgs.entr}/bin/entr sh -c \
             'cabal v1-configure --enable-tests && \
                cabal v1-build test && dist/build/test/test'
      }
      function doctest-runner () {
         ${pkgs.ag}/bin/ag -l | \
           ${pkgs.entr}/bin/entr sh -c \
             'cabal v1-configure --enable-tests && \
                cabal v1-build doctests && dist/build/doctests/doctests'
      }
      function repl () {
        ${pkgs.cabal-install}/bin/cabal v1-repl lib:arrayfire
      }
      function docs () {
        ${pkgs.cabal-install}/bin/cabal haddock
        open ./dist-newstyle/*/*/*/*/doc/html/arrayfire/index.html
      }
    '';
  })
