let pkgs = import <nixpkgs> {};

in { website = pkgs.haskellPackages.callCabal2nix "website" (pkgs.lib.cleanSource ./.) {}; }
