with (import ./default.nix {});

pkgs.lib.overrideDerivation hsPkgs.arrayfire.env (drv: {
  shellHook = ''
    export AF_PRINT_ERRORS=1
    export PATH=$PATH:${hsPkgs.doctest}/bin
    export PATH=$PATH:${pkgs.cabal-install}/bin

    export AF_LIB=${pkgs.arrayfire}/lib
    export AF_INCLUDE=${pkgs.arrayfire}/include
    export LD_LIBRARY_PATH="$AF_LIB:$LD_LIBRARY_PATH"

    echo $LD_LIBRARY_PATH

    export RUN_HS="runhaskell Setup.hs"
    export CFLAGS="-f+disable-default-paths --extra-include-dirs=$AF_INCLUDE --extra-lib-dirs=$AF_LIB"
    export CONFIGURE="$RUN_HS configure $CFLAGS"

    function ghcid () {
      $CONFIGURE && \
        ${pkgs.ghcid}/bin/ghcid \
          -c '$RUN_HS repl --repl-options=-fno-code lib:arrayfire'
    }

    function test-runner () {
      $CONFIGURE --enable-tests && \
        ${pkgs.ag}/bin/ag -l | \
          ${pkgs.entr}/bin/entr sh -c \
            '$RUN_HS build test && dist/build/test/test'
    }

    function test() {
      $CONFIGURE --enable-tests && \
        $RUN_HS build test && dist/build/test/test
    }

    function doctest-runner () {
      ${pkgs.ag}/bin/ag -l | \
        ${pkgs.entr}/bin/entr sh -c \
          '$CONFIGURE --enable-tests && \
            $RUN_HS build doctests && \
              dist/build/doctests/doctests src/ArrayFire/Algorithm.hs'
    }
    function exe () {
        cabal run main
    }
    function repl () {
      $CONFIGURE && $RUN_HS repl lib:arrayfire
    }
    function docs () {
      cabal haddock
      open ./dist-newstyle/*/*/*/*/doc/html/arrayfire/index.html
    }
  '';
})
