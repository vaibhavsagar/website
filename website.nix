{ mkDerivation, base, filepath, hakyll, lib }:
mkDerivation {
  pname = "website";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base filepath hakyll ];
  license = lib.licenses.bsd3;
}
