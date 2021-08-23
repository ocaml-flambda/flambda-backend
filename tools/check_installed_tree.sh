#!/bin/sh

set -e -u -o pipefail

prefix="$1"
shift

built=$(mktemp)
installed=$(mktemp)

trap "rm -f ${built} ${installed}" EXIT

list_artifacts() {
    directory="$1"
    (cd "${directory}";
     find . -type d \( -path ./compiler-libs -o -path ./dynlink/dynlink_compilerlibs \) -prune -false \
          -o -name \*.cmi \
          -o -name \*.cmt \
          -o -name \*.cmti \
          -o -name \*.cmx \
         | xargs basename)
}

list_artifacts "_build0/stdlib" > "${built}"
list_artifacts "_build0/otherlibs" >> "${built}"
list_artifacts "${prefix}/lib/ocaml" > "${installed}"

sort -o "${built}" "${built}"
sort -o "${installed}" "${installed}"

echo "Differences between built and installed artifacts:"
diff "${built}" "${installed}" && echo "(none)"

