# --argstr compiler could be ghcjs or ghc
{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc"
, enableLibraryProfiling ? false
}:
let
  inherit (nixpkgs) pkgs;

  tryReflex = import (pkgs.fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "1670c5b899658babeda58329d3df6b943cf6aeca";
    sha256 = "0ry3fcxiqr43c5fghsiqn0iarj4gfvk77jkc4na7j7r3k8vjdjh2";
  }) {
    inherit enableLibraryProfiling;
  };

  # compiler could be ghcjs or ghc
  haskellPackages = tryReflex.${compiler};

  drv = haskellPackages.callPackage ./rule110.nix {};

in

  if pkgs.lib.inNixShell then drv.env else drv
