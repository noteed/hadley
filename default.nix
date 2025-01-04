{ mkDerivation, base, blaze-html, cmdargs, data-default, directory
, filepath, ghc-lib-parser, hlint, lib, pandoc, process, text
}:
mkDerivation {
  pname = "hadley";
  version = "0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    base blaze-html cmdargs data-default directory filepath
    ghc-lib-parser hlint pandoc process text
  ];
  description = "Hadley renders static pages for Haskell projects";
  license = lib.licenses.bsd3;
  mainProgram = "hadley";
}
