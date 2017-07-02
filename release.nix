let
  pkgs        = import <nixpkgs> {};
  website-drv = pkgs.haskellPackages.haskellSrc2nix {
    name = "website";
    src  = pkgs.lib.cleanSource ./.;
  };

in { website = pkgs.haskellPackages.callPackage website-drv {}; }
