let
  inherit (import <nixpkgs> {}) fetchFromGitHub lib;
  # ./updater versions.json nixpkgs nixos-17.09
  versions = lib.mapAttrs
    (_: fetchFromGitHub)
    (builtins.fromJSON (builtins.readFile ./versions.json));
  inherit (builtins) any filterSource;
  nixpkgs = import versions.nixpkgs {};
  sourceFilter = src: name: type: let
    relPath = lib.removePrefix (toString src + "/") (toString name);
  in lib.cleanSourceFilter name type && (any (lib.flip lib.hasPrefix relPath) [
    "site.hs" "website.cabal" "LICENSE"
  ]);
  drv = nixpkgs.haskellPackages.callCabal2nix "website" (filterSource (sourceFilter ./.) ./.) {};
in if nixpkgs.lib.inNixShell then drv.env else drv
