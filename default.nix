{pkgs ? import ./nix/pkgs.nix {}}:
pkgs.haskellPackages.callPackage ./acetone.nix {}
