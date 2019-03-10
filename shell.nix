{pkgs ? import ./nix/pkgs.nix {}}:
let root = import ./. {pkgs = pkgs;}; in
{
    library = root.library.env.overrideAttrs (p: {
        nativeBuildInputs = p.nativeBuildInputs ++ [
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.ghcid
        ];
    });
}
