let
  # ./updater versions.json nixpkgs nixos-18.03
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/tarball/${rev}";
  };
  nixpkgs = import (fetcher (builtins.fromJSON (builtins.readFile ./versions.json)).nixpkgs) { config.allowBroken = true; };
  lib = nixpkgs.lib;
  myFilter = ls: src: name: type: let
    relPath = lib.removePrefix (toString src + "/") (toString name);
  in lib.cleanSourceFilter name type && (builtins.any (lib.flip lib.hasPrefix relPath) ls);
  contentFilter = myFilter [ "blog" "css" "drafts" "extra" "index.html" "pages" "templates" ];
  overrides = self: super: {
    hakyll = nixpkgs.haskell.lib.overrideCabal super.hakyll {
      patches = [];
      doCheck = false;
    };
  };
  drv = (nixpkgs.haskellPackages.override { inherit overrides; }).callPackage ./website.nix {};
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
