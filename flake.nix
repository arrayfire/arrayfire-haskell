{
  description = "arrayfire/arrayfire-haskell: ArrayFire Haskell bindings";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    arrayfire-nix = {
      url = "github:twesterhout/arrayfire-nix";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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

      # An overlay that lets us test arrayfire-haskell with different GHC versions
      arrayfire-haskell-overlay = self: super: {
        haskell = super.haskell // {
          packageOverrides = inputs.nixpkgs.lib.composeExtensions super.haskell.packageOverrides
            (hself: hsuper: {
              arrayfire = self.haskell.lib.appendConfigureFlags
                (hself.callCabal2nix "arrayfire" src { af = self.arrayfire; })
                [ "-f disable-default-paths" ];
            });
        };
      };

      devShell-for = pkgs:
        let
          ps = pkgs.haskellPackages;
        in
        ps.shellFor {
          packages = ps: with ps; [ arrayfire ];
          withHoogle = true;
          buildInputs = with pkgs; [ ocl-icd ];
          nativeBuildInputs = with pkgs; with ps; [
            # Building and testing
            cabal-install
            doctest
            hsc2hs
            hspec-discover
            # Language servers
            haskell-language-server
            nil
            # Formatters
            nixpkgs-fmt
          ];
          shellHook = ''
          '';
        };

      pkgs-for = system: import inputs.nixpkgs {
        inherit system;
        overlays = [
          inputs.arrayfire-nix.overlays.default
          arrayfire-haskell-overlay
        ];
      };
    in
    {
      packages = inputs.flake-utils.lib.eachDefaultSystemMap (system:
        with (pkgs-for system); {
          default = haskellPackages.arrayfire;
          haskell = haskell.packages;
        });

      devShells = inputs.flake-utils.lib.eachDefaultSystemMap (system: {
        default = devShell-for (pkgs-for system);
      });

      overlays.default = arrayfire-haskell-overlay;
    };
}
