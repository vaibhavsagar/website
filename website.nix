{ mkDerivation, base, blaze-html, filepath, hakyll, ghc-syntax-highlighter, pandoc, text, lib }:
mkDerivation {
  pname = "website";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base blaze-html filepath hakyll ghc-syntax-highlighter pandoc text ];
  license = lib.licenses.bsd3;
}
