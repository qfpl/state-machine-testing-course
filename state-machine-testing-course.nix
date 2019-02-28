{ mkDerivation, base, hedgehog, lens, mtl, stdenv, tasty
, tasty-hedgehog
}:
mkDerivation {
  pname = "state-machine-testing-course";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens mtl ];
  testHaskellDepends = [ base hedgehog lens tasty tasty-hedgehog ];
  license = stdenv.lib.licenses.bsd3;
}
