{ nixpkgs ? import <nixpkgs> {} }:
let
  drv = nixpkgs.haskellPackages.callCabal2nix "website" (nixpkgs.lib.cleanSource ./.) {};
in if nixpkgs.lib.inNixShell then drv.env else drv
