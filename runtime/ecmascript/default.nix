{stdenv, bash, makeWrapper}:
stdenv.mkDerivation {
    name = "ecmascript";
    src = ./.;
    phases = ["unpackPhase" "buildPhase" "installPhase"];
    buildInputs = [
        bash
        makeWrapper
    ];
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
        mkdir -p "$out/bin" "$out/share/bin" "$out/share/lib"

        mv 'link.bash' "$out/share/bin"
        mv 'runtime.js' "$out/share/lib"

        makeWrapper '${bash}/bin/bash' "$out/bin/link"                      \
            --add-flags "$out/share/bin/link.bash"                          \
            --add-flags "$out/share/lib/runtime.js"
    '';
}
