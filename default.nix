let
  # ./updater versions.json nixpkgs nixos-18.03
  fetcher = { owner, repo, rev, sha256 }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
  };
  nixpkgs = import (fetcher (builtins.fromJSON (builtins.readFile ./versions.json)).nixpkgs) {};
  lib = nixpkgs.lib;
  sourceFilter = src: name: type: let
    relPath = lib.removePrefix (toString src + "/") (toString name);
  in lib.cleanSourceFilter name type && (builtins.any (lib.flip lib.hasPrefix relPath) [
    "site.hs" "website.cabal" "LICENSE"
  ]);
  drv = nixpkgs.haskellPackages.callCabal2nix "website" (builtins.filterSource (sourceFilter ./.) ./.) {};
in if lib.inNixShell then drv.env else drv
