{ mkDerivation, base, containers, hedgehog, lens, mtl, stdenv
, tasty, tasty-hedgehog
}:
mkDerivation {
  pname = "state-machine-testing-course";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers lens mtl ];
  testHaskellDepends = [
    base containers hedgehog lens tasty tasty-hedgehog
  ];
  description = "Introductory course for property-based state-machine testing";
  license = stdenv.lib.licenses.bsd3;
}
