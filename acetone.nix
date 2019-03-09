{mkDerivation, base, containers, hspec, lens, parallel, QuickCheck}:
mkDerivation {
    pname = "acetone";
    version = "0.0.0.0";
    license = null;
    src = builtins.filterSource (p: t: p != toString ./dist &&
                                       p != toString ./result &&
                                       p != toString ./.git)
                                ./.;
    buildDepends = [
        base
        containers
        hspec
        lens
        parallel
        QuickCheck
    ];
}
