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
  contentFilter = myFilter [ "blog" "css" "drafts" "extra" "index.html" "pages" "templates" ];
  drv = nixpkgs.haskellPackages.callPackage ./website.nix {};
  site = nixpkgs.runCommand "site" {
    buildInputs = drv.env.nativeBuildInputs;
    src = builtins.filterSource (contentFilter ./.) ./.;
    LANG = "C.UTF-8";
  } ''
    workdir=$(${nixpkgs.coreutils}/bin/mktemp -d)
    cp -R $src/* $workdir
    cd $workdir
    mkdir -p $out
    ln -sfn $out _site
    runhaskell ${./site.hs} build
  '';
in site
