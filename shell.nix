{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.runCommand "dummy" {
  buildInputs = [
    (nixpkgs.haskellPackages.ghcWithPackages (p: [ p.hakyll p.filepath ]))
  ];
} ""
