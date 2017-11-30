{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (builtins) any filterSource;
  lib = nixpkgs.lib;
  sourceFilter = src: name: type: let
    relPath = lib.removePrefix (toString src + "/") (toString name);
  in lib.cleanSourceFilter name type && (any (lib.flip lib.hasPrefix relPath) [
    "site.hs" "website.cabal" "LICENSE"
  ]);
  drv = nixpkgs.haskellPackages.callCabal2nix "website" (filterSource (sourceFilter ./.) ./.) {};
in if nixpkgs.lib.inNixShell then drv.env else drv
