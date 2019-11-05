{ pkgs ? import ./nix/nixpkgs.nix
, compiler ? "ghc865"
}:

with pkgs;

with rec {
  hsPkgs = haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      arrayfire = hsLib.overrideCabal (hself.callPackage ./pkg.nix { af = pkgs.arrayfire; }) (old: {
        configureFlags = (old.configureFlags or []) ++ [
          "-f+disable-default-paths"
          "--extra-include-dirs=${arrayfire}/include"
          "--extra-lib-dirs=${arrayfire}/lib"
        ];

        doCheck = true;
        doHaddock = false;
      });
    };
  };

  hsLib = haskell.lib;
};

rec {
  inherit compiler pkgs hsPkgs;
}
