{ mkDerivation, base, hedgehog, lens, mtl, stdenv }:
mkDerivation {
  pname = "fp-eedee";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens mtl ];
  testHaskellDepends = [ base hedgehog lens ];
  license = stdenv.lib.licenses.bsd3;
}
