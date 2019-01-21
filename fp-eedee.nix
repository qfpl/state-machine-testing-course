{ mkDerivation, base, bytestring, containers, haskeline, hedgehog
, lens, mtl, process, stdenv, turtle, typed-process, vector
}:
mkDerivation {
  pname = "fp-eedee";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers haskeline lens mtl typed-process vector
  ];
  testHaskellDepends = [
    base bytestring hedgehog lens process turtle typed-process
  ];
  doHaddock = false;
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
