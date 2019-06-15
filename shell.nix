let
  pkgs = import <nixpkgs> {
  };
  hspkgs = pkgs.haskell.packages.ghc843;

  f = import ./default.nix;
  drv = hspkgs.callPackage f {};
in

pkgs.stdenv.mkDerivation {
  name = "hadley-env";
  buildInputs = [
    pkgs.postgresql
  ] ++ drv.env.nativeBuildInputs;
  shellHook = ''
    echo Entering the Nix environment for Hadley...
    nix --version
    ghc --version
    # ghci -ghci-script ghci-nix-shell.conf
  '';
}
