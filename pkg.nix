{ mkDerivation, base, directory, parsec, stdenv, text, vector
, hspec, hspec-discover
}:
mkDerivation {
  pname = "arrayfire";
  version = "0.4.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base vector ];
  executableHaskellDepends = [ base directory parsec text ];
  testHaskellDepends = [ hspec hspec-discover ];
  homepage = "https://github.com/arrayfire/arrayfire-haskell";
  description = "Haskell bindings to ArrayFire";
  license = stdenv.lib.licenses.bsd3;
}
