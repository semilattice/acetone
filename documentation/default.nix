{stdenv, python3Packages}:
stdenv.mkDerivation {
    name = "documentation";
    src = ./.;
    buildInputs = [
        python3Packages.sphinx
    ];
    phases = ["unpackPhase" "buildPhase" "installPhase"];
    buildPhase = ''
        sphinx-build -M html . .
    '';
    installPhase = ''
        mkdir -p "$out/share/doc"
        mv 'html' "$out/share/doc"
    '';
}
