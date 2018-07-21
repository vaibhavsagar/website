let
  # ./updater versions.json nixpkgs nixos-18.03
  fetcher = { owner, repo, rev, sha256 }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
  };
  nixpkgs = import (fetcher (builtins.fromJSON (builtins.readFile ./versions.json)).nixpkgs) {};
  lib = nixpkgs.lib;
  relPath = src: name: lib.removePrefix (toString src + "/") (toString name);
  sourceFilter = src: name: type: lib.cleanSourceFilter name type && (builtins.any (lib.flip lib.hasPrefix (relPath src name)) [
    "site.hs" "website.cabal" "LICENSE"
  ]);
  contentFilter = src: name: type: lib.cleanSourceFilter name type && (builtins.any (lib.flip lib.hasPrefix (relPath src name)) [
    "blog" "css" "drafts" "extra" "index.html" "pages" "templates"
  ]);
  drv = nixpkgs.haskellPackages.callCabal2nix "website" (builtins.filterSource (sourceFilter ./.) ./.) {};
  site = nixpkgs.runCommand "site" {
    buildInputs = [ drv nixpkgs.glibcLocales ];
    src = builtins.filterSource (contentFilter ./.) ./.;
    LC_ALL = "en_US.UTF-8";
  } ''
    workdir=$(${nixpkgs.coreutils}/bin/mktemp -d)
    cp -R $src/* $workdir
    cd $workdir
    site build
    mkdir -p $out
    cp -R _site/* $out
  '';
in if lib.inNixShell then drv.env else site
