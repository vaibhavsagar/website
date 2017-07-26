let pkgs = import <nixpkgs> {};
in pkgs.haskellPackages.callCabal2nix "website" (pkgs.lib.cleanSource ./.) {}
