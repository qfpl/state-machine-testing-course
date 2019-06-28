{ mkDerivation, base, hedgehog, hedgehog-fn, lens, mtl, stdenv
, tasty, tasty-hedgehog
}:
mkDerivation {
  pname = "state-machine-testing-course";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens mtl ];
  testHaskellDepends = [
    base hedgehog hedgehog-fn lens tasty tasty-hedgehog
  ];
  description = "Introductory course for property-based state-machine testing";
  license = stdenv.lib.licenses.bsd3;
}
