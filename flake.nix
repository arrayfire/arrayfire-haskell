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

      # Build ArrayFire from the official binary installer; avoids freeimage entirely.
      mkArrayfire = pkgs: pkgs.stdenv.mkDerivation rec {
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
        meta = {
          description = "A general-purpose library for parallel and massively-parallel architectures";
          platforms = [ "x86_64-linux" ];
        };
      };

      arrayfire-overlay = self: super: {
        arrayfire = mkArrayfire self;
      };

      # An overlay that lets us test arrayfire-haskell with different GHC versions
      arrayfire-haskell-overlay = self: super: {
        haskell = super.haskell // {
          packageOverrides = inputs.nixpkgs.lib.composeExtensions super.haskell.packageOverrides
            (hself: hsuper: {
              arrayfire = self.haskell.lib.appendConfigureFlags
                (hself.callCabal2nix "arrayfire" src {
                  af = self.arrayfire;
                })
                [ "-f disable-default-paths" ];
            });
        };
      };

      devShell-for = pkgs:
        let
          ps = pkgs.haskellPackages;
          isLinux = pkgs.stdenv.isLinux;
          isDarwin = pkgs.stdenv.isDarwin;
        in
          ps.shellFor {
            packages = ps: if isLinux then [ ps.arrayfire ] else [ ];
            withHoogle = true;
            buildInputs = with pkgs; (if isLinux then [ ocl-icd ] else [ darwin.apple_sdk.frameworks.Security ]);
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
            shellHook = if isLinux then ''export LD_LIBRARY_PATH="${pkgs.arrayfire}/lib:$LD_LIBRARY_PATH"'' else "";
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
        with (pkgs-for system); {
          default = haskellPackages.arrayfire;
        });

      devShells = inputs.flake-utils.lib.eachDefaultSystemMap (system: {
        default = devShell-for (pkgs-for system);
      });

      overlays.default = arrayfire-haskell-overlay;
    };
}
