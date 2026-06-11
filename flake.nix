{
  description = "arrayfire/arrayfire-haskell: ArrayFire Haskell bindings";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = inputs:
    let
      src = inputs.nix-filter.lib {
        root = ./.;
        include = [
          "cbits"
          "exe"
          "gen"
          "include"
          "src"
          "test"
          "arrayfire.cabal"
          "README.md"
          "CHANGELOG.md"
          "LICENSE"
        ];
      };

      # Build ArrayFire from the official Linux binary installer; avoids freeimage entirely.
      mkArrayfireLinux = pkgs: pkgs.stdenv.mkDerivation rec {
        pname = "arrayfire";
        version = "3.10.0";
        src = pkgs.fetchurl {
          url = "https://arrayfire.s3.amazonaws.com/${version}/ArrayFire-v${version}_Linux_x86_64.sh";
          hash = "sha256-8SibCWnRxts79S6WEHb3skF2TIDl1QnjY6EiohmoIog=";
        };
        nativeBuildInputs = [ pkgs.autoPatchelfHook ];
        # GPU/visualization libs (CUDA, OpenGL, GLFW, FreeImage, Intel OpenCL) are
        # optional ArrayFire backends not available in headless CI environments.
        autoPatchelfIgnoreMissingDeps = true;
        buildInputs = with pkgs; [
          stdenv.cc.cc.lib
          fftw
          fftwFloat
          openblas
          ocl-icd
          boost.out
        ];
        unpackPhase = "true";
        installPhase = ''
          mkdir -p $out
          bash $src --exclude-subdir --prefix=$out
        '';
        # autoPatchelfIgnoreMissingDeps silences missing-dep errors at build time,
        # but a genuinely-absent dep of libafcpu.so would still make its runtime
        # dlopen fail with LoadLibError. Fail the build loudly if the CPU backend
        # has any unresolved (=> not just intentionally-ignored GPU) dependencies.
        doInstallCheck = true;
        installCheckPhase = ''
          libdir=$out/lib64
          [ -d "$libdir" ] || libdir=$out/lib
          cpu=$(echo "$libdir"/libafcpu.so* | tr ' ' '\n' | head -n1)
          echo "Checking runtime deps of $cpu"
          if ldd "$cpu" | grep -i 'not found'; then
            echo "ERROR: libafcpu.so has unresolved dependencies" >&2
            exit 1
          fi
        '';
        meta = {
          description = "A general-purpose library for parallel and massively-parallel architectures";
          platforms = [ "x86_64-linux" ];
        };
      };

      # Build ArrayFire on macOS from the official .pkg installer. ArrayFire has not
      # shipped a macOS binary since 3.8.2 (x86_64 only), so darwin pins that version.
      # The .pkg is a xar archive of component sub-packages, each carrying a
      # gzip+cpio Payload that installs under opt/arrayfire/{include,lib}.
      mkArrayfireDarwin = pkgs: pkgs.stdenv.mkDerivation rec {
        pname = "arrayfire";
        version = "3.8.2";
        src = pkgs.fetchurl {
          url = "https://arrayfire.s3.amazonaws.com/${version}/ArrayFire-${version}_OSX_x86_64.pkg";
          hash = "sha256-MDqpDONbzl+PNu2VS1UTaYL10fpzpt0pv10oxNwgm+k=";
        };
        nativeBuildInputs = with pkgs; [ xar cpio fixDarwinDylibNames ];
        # Never strip the prebuilt vendor dylibs: the default strip phase corrupts
        # them (it silently truncated libmkl_core.dylib to 0 bytes, which then made
        # MKL fail to load its computational layer at runtime).
        dontStrip = true;
        unpackPhase = ''
          runHook preUnpack
          xar -xf $src
          runHook postUnpack
        '';
        # Extract every component Payload (except the heavy CUDA/OpenCL/examples ones
        # we don't ship) into a staging tree, then install only the unified + CPU
        # backends and their bundled runtime deps (MKL, TBB, forge).
        installPhase = ''
          runHook preInstall
          mkdir -p stage
          for comp in ArrayFire-${version}-Darwin-*.pkg; do
            case "$comp" in
              *cuda*|*opencl*|*examples*|*documentation*) continue ;;
            esac
            [ -f "$comp/Payload" ] || continue
            ( cd stage && gzip -dc "../$comp/Payload" | cpio -id --quiet )
          done

          mkdir -p $out/lib
          cp -R stage/opt/arrayfire/include $out/include
          for pat in 'libaf.*' 'libafcpu.*' 'libforge.*' 'libmkl_*.dylib' \
                     'libtbb*.dylib' 'libiomp*.dylib'; do
            cp -P stage/opt/arrayfire/lib/$pat $out/lib/ 2>/dev/null || true
          done
          runHook postInstall
        '';
        # fixDarwinDylibNames (run in fixupPhase) rewrites the @rpath install ids
        # and matching inter-library references to absolute store paths. It only
        # rewrites references whose leaf matches a sibling's *original* id, so it
        # misses cases where the ids differ, e.g. libafcpu -> @rpath/libmkl_rt and
        # libmkl_tbb_thread -> @rpath/libtbb (the latter is dlopen'd by MKL's
        # libmkl_rt and would otherwise fail to load at runtime). Re-point any
        # remaining @rpath/<leaf> dep at $out/lib/<leaf> so everything is hermetic.
        postFixup = ''
          for dylib in $out/lib/*.dylib; do
            for dep in $(otool -L "$dylib" | awk 'NR>1{print $1}' | grep '^@rpath/' || true); do
              leaf=''${dep#@rpath/}
              if [ -e "$out/lib/$leaf" ]; then
                install_name_tool -change "$dep" "$out/lib/$leaf" "$dylib"
              fi
            done
          done
        '';
        meta = {
          description = "A general-purpose library for parallel and massively-parallel architectures";
          platforms = [ "x86_64-darwin" ];
        };
      };

      mkArrayfire = pkgs:
        if pkgs.stdenv.isDarwin
        then mkArrayfireDarwin pkgs
        else mkArrayfireLinux pkgs;

      arrayfire-overlay = self: super: {
        arrayfire = mkArrayfire self;
      };

      # An overlay that lets us test arrayfire-haskell with different GHC versions
      arrayfire-haskell-overlay = self: super: {
        haskell = super.haskell // {
          packageOverrides = inputs.nixpkgs.lib.composeExtensions super.haskell.packageOverrides
            (hself: hsuper: {
              arrayfire =
                let
                  pkg = self.haskell.lib.appendConfigureFlags
                    (hself.callCabal2nix "arrayfire" src {
                      af = self.arrayfire;
                    })
                    [ "-f disable-default-paths" ];
                in
                  # On macOS ArrayFire's bundled MKL dlopens its threading layer
                  # (libmkl_tbb_thread.dylib) by bare leaf name, which dyld only
                  # resolves via DYLD_LIBRARY_PATH. Point it at the arrayfire libs
                  # so the test suite (and doctests) can run. Runtime consumers of
                  # this package need the same DYLD_LIBRARY_PATH.
                  if self.stdenv.isDarwin
                  then pkg.overrideAttrs (old: {
                    preCheck = (old.preCheck or "") + ''
                      export DYLD_LIBRARY_PATH="${self.arrayfire}/lib''${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}"
                    '';
                  })
                  # On Linux we link against the unified backend (libaf), which is
                  # just a dispatcher that dlopens the real backend impl
                  # (libafcpu.so) at runtime. The sandboxed check phase has no
                  # LD_LIBRARY_PATH/AF_PATH, so that dlopen finds nothing and every
                  # test throws AFException LoadLibError (501). Point the loader at
                  # the arrayfire libs so the backend can be found.
                  else pkg.overrideAttrs (old: {
                    preCheck = (old.preCheck or "") + ''
                      export AF_PATH="${self.arrayfire}"
                      export LD_LIBRARY_PATH="${self.arrayfire}/lib:${self.arrayfire}/lib64''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
                    '';
                  });
            });
        };
      };

      devShell-for = pkgs:
        let
          ps = pkgs.haskellPackages;
          isLinux = pkgs.stdenv.isLinux;
          isDarwin = pkgs.stdenv.isDarwin;
          # ArrayFire only ships an x86_64 macOS binary, so it's unavailable on
          # Apple Silicon; fall back to a plain shell there.
          hasArrayfire = isLinux || pkgs.stdenv.hostPlatform.system == "x86_64-darwin";
        in
          ps.shellFor {
            packages = ps: if hasArrayfire then [ ps.arrayfire ] else [ ];
            withHoogle = true;
            buildInputs = with pkgs; (if isLinux then [ ocl-icd ] else [ ]);
            nativeBuildInputs = with pkgs; with ps; [
              # Building and testing
              cabal-install
              doctest
              hsc2hs
              # hspec-discover
              nil
              # Formatters
              nixpkgs-fmt
            ];
            shellHook =
              if isLinux then ''export LD_LIBRARY_PATH="${pkgs.arrayfire}/lib:$LD_LIBRARY_PATH"''
              else if hasArrayfire then ''export DYLD_LIBRARY_PATH="${pkgs.arrayfire}/lib:$DYLD_LIBRARY_PATH"''
              else "";
          };

      pkgs-for = system: import inputs.nixpkgs {
        inherit system;
        overlays = [
          arrayfire-overlay
          arrayfire-haskell-overlay
        ];
      };
    in
    {
      packages = inputs.flake-utils.lib.eachDefaultSystemMap (system:
        let
          pkgs = pkgs-for system;
          # ArrayFire only provides binaries for x86_64-linux and x86_64-darwin
          # (no Apple Silicon / aarch64), so only expose the package there.
          hasArrayfire = pkgs.stdenv.isLinux || system == "x86_64-darwin";
        in inputs.nixpkgs.lib.optionalAttrs hasArrayfire {
          default = pkgs.haskellPackages.arrayfire;
        });

      devShells = inputs.flake-utils.lib.eachDefaultSystemMap (system: {
        default = devShell-for (pkgs-for system);
      });

      overlays.default = arrayfire-haskell-overlay;
    };
}
