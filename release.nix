let
  pkgs = import <nixpkgs> {
  };
  hspkgs = pkgs.haskell.packages.ghc810;
in
{
  hadley = hspkgs.callPackage ./default.nix {};
}
