{ mkDerivation, base, directory, parsec, stdenv, text }:
mkDerivation {
  pname = "fire";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base directory parsec text ];
  homepage = "https://github.com/dmjio/fire";
  description = "Haskell bindings to ArrayFire";
  license = stdenv.lib.licenses.bsd3;
}
