{stdenv}:
stdenv.mkDerivation {
    name = "ecmascript";
    src = ./.;
    phases = ["unpackPhase" "buildPhase" "installPhase"];
    buildPhase = ''
        {
            echo '// Begin of Acetone runtime library.'
            echo 'var AR = {};'
            echo 'var AG = {};'
            cat 'panic.js'
            cat 'lazy.js'
            cat 'effect.js'
            echo '// End of Acetone runtime library.'
            echo '// Put Acetone output after this line.'
        } > 'runtime.js'
    '';
    installPhase = ''
        mkdir -p "$out/share/lib"
        mv 'runtime.js' "$out/share/lib"
    '';
}
