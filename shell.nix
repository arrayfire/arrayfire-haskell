{ pkgs ? import <nixpkgs> {} }:
let
  pkg = (import ./default.nix {}).env;
in
  pkgs.lib.overrideDerivation pkg (drv: {
    shellHook = ''
      export AF_PRINT_ERRORS=1
      export PATH=$PATH:${pkgs.haskellPackages.doctest}/bin
      export PATH=$PATH:${pkgs.haskellPackages.cabal-install}/bin
      function ghcid () {
        ${pkgs.haskellPackages.ghcid.bin}/bin/ghcid -c 'cabal v1-repl lib:arrayfire'
      };
      function test-runner () {
         ${pkgs.silver-searcher}/bin/ag -l | \
           ${pkgs.entr}/bin/entr sh -c \
             'cabal v1-configure --enable-tests && \
                cabal v1-build test && dist/build/test/test'
      }
      function doctest-runner () {
         ${pkgs.silver-searcher}/bin/ag -l | \
           ${pkgs.entr}/bin/entr sh -c \
             'cabal v1-configure --enable-tests && \
                cabal v1-build doctests && dist/build/doctests/doctests src/ArrayFire/Algorithm.hs'
      }
      function exe () {
         cabal run main
      }
      function repl () {
        cabal v1-repl lib:arrayfire
      }
      function docs () {
        cabal haddock
        open ./dist-newstyle/*/*/*/*/doc/html/arrayfire/index.html
      }
      function upload-docs () {
        cabal haddock --haddock-for-hackage
        cabal upload -d dist-newstyle/arrayfire-*.*.*.*-docs.tar.gz --publish
      }
    '';
  })
