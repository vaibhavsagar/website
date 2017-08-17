(import <nixpkgs> {}).runCommand "dummy" {
  buildInputs = [ (import ./release.nix) ];
} ""
