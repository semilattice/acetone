{pkgs ? import ./nix/pkgs.nix {}}:
{
    library = pkgs.haskellPackages.callPackage ./library {};
    runtime = pkgs.callPackage ./runtime {};
}
