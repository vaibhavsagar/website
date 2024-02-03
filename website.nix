{ mkDerivation, base, blaze-html, filepath, ghc-syntax-highlighter
, hakyll, lib, pandoc, pandoc-types, text
}:
mkDerivation {
  pname = "website";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base blaze-html filepath ghc-syntax-highlighter hakyll pandoc
    pandoc-types text
  ];
  license = lib.licenses.bsd3;
  mainProgram = "site";
}
