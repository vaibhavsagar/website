let pkgs = import <nixpkgs> { };

in rec {
  website = pkgs.haskellPackages.callPackage ./default.nix {};
}
