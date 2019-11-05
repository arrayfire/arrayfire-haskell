{
  ghc844 = (import ./default.nix { compiler = "ghc844"; }).hsPkgs.arrayfire;
  ghc865 = (import ./default.nix { compiler = "ghc865"; }).hsPkgs.arrayfire;
  ghc881 = (import ./default.nix { compiler = "ghc881"; }).hsPkgs.arrayfire;
}
