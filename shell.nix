{ nixpkgs ? import <nixpkgs> {} }:
(nixpkgs.haskellPackages.callCabal2nix
  "website"
  (nixpkgs.lib.cleanSource ./.)
  {}
).env
