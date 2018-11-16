{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "fire";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/dmjio/fire";
  description = "Haskell bindings to ArrayFire";
  license = stdenv.lib.licenses.bsd3;
}
