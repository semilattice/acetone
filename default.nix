{pkgs ? import ./nix/pkgs.nix {}}:
{
    documentation = pkgs.callPackage ./documentation {};
    library = pkgs.haskellPackages.callPackage ./library {};
    runtime = pkgs.callPackage ./runtime {};
}
