{
  inputs.nixpkgs-src.url = "github:NixOS/nixpkgs/release-23.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nix-filter.url = "github:numtide/nix-filter";
  inputs.flake-compat.url = "github:edolstra/flake-compat";
  outputs = {nixpkgs-src, flake-utils, nix-filter, ...}:
    flake-utils.lib.eachDefaultSystem (system: let
      nixpkgs = import nixpkgs-src { inherit system; };
      content = nix-filter {
        root = ./.;
        include = [ "blog" "css" "drafts" "extra" "index.html" "pages" "templates" ];
      };
      drv = nixpkgs.haskellPackages.callPackage ./website.nix {};
      site = nixpkgs.runCommand "site" {
        buildInputs = drv.env.nativeBuildInputs;
        src = content;
        LANG = "C.UTF-8";
      } ''
        workdir=$(${nixpkgs.coreutils}/bin/mktemp -d)
        cp -R $src/* $workdir
        cd $workdir
        mkdir -p $out
        ln -sfn $out _site
        runhaskell ${./site.hs} build
      '';
    in {
      packages.website = drv;
      defaultPackage = site;
      devShell = site;
    });
}
