let
  pkgs = import <nixpkgs> {
  };
  hspkgs = pkgs.haskell.packages.ghc843;
in
{
  hadley = hspkgs.callPackage ./default.nix {};
}
