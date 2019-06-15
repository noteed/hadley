{ mkDerivation, base, blaze-html, cmdargs, data-default, directory
, filepath, haskell-src-exts, hlint, pandoc, process, stdenv, text
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
    haskell-src-exts hlint pandoc process text
  ];
  description = "Hadley renders static pages for Haskell projects";
  license = stdenv.lib.licenses.bsd3;
}
