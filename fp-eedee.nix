{ mkDerivation, base, bytestring, hedgehog, lens, mtl, process
, stdenv, text, turtle, typed-process, unix
}:
mkDerivation {
  pname = "fp-eedee";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens mtl ];
  testHaskellDepends = [
    base bytestring hedgehog lens process text turtle typed-process
    unix
  ];
  license = stdenv.lib.licenses.bsd3;
}
