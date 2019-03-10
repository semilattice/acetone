set -o nounset

runtime="$1"
units=("${@:2}")

cat "$runtime" "${units[@]}"
