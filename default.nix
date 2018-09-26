let
  # ./updater versions.json nixpkgs nixos-18.03
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
  };
  nixpkgs = import (fetcher (builtins.fromJSON (builtins.readFile ./versions.json)).nixpkgs) {};
  lib = nixpkgs.lib;
  myFilter = ls: src: name: type: let
    relPath = lib.removePrefix (toString src + "/") (toString name);
  in lib.cleanSourceFilter name type && (builtins.any (lib.flip lib.hasPrefix relPath) ls);
  sourceFilter = myFilter [ "site.hs" "website.cabal" "LICENSE" ];
  contentFilter = myFilter [ "blog" "css" "drafts" "extra" "index.html" "pages" "templates" ];
  drv = nixpkgs.haskellPackages.callCabal2nix "website" (builtins.filterSource (sourceFilter ./.) ./.) {};
  site = nixpkgs.runCommand "site" {
    buildInputs = [ nixpkgs.glibcLocales ] ++ drv.env.nativeBuildInputs;
    src = builtins.filterSource (contentFilter ./.) ./.;
    LC_ALL = "en_US.UTF-8";
  } ''
    workdir=$(${nixpkgs.coreutils}/bin/mktemp -d)
    cp -R $src/* $workdir
    cd $workdir
    runhaskell ${./site.hs} build
    mkdir -p $out
    cp -R _site/* $out
  '';
in site
