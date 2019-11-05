{ mkDerivation, af, base, directory, filepath, hspec
, hspec-discover, parsec, QuickCheck, quickcheck-classes, stdenv
, text, vector
}:
mkDerivation {
  pname = "arrayfire";
  version = "0.4.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base filepath vector ];
  librarySystemDepends = [ af ];
  executableHaskellDepends = [ base directory parsec text vector ];
  testHaskellDepends = [
    base directory hspec hspec-discover QuickCheck quickcheck-classes
    vector
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/arrayfire/arrayfire-haskell";
  description = "Haskell bindings to the ArrayFire general-purpose GPU library";
  license = stdenv.lib.licenses.bsd3;
}
