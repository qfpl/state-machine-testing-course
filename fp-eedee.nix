{ mkDerivation, base, hedgehog, lens, process, stdenv, text, turtle
, typed-process, unix
}:
mkDerivation {
  pname = "fp-eedee";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [
    base hedgehog lens process text turtle typed-process unix
  ];
  doHaddock = false;
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
