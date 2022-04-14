#!/bin/bash

# To be run from the root of the Flambda backend repo.
# Cherry pick a commit from upstream repo and apply it to flambda-backend repo.
#
# Intended for experimental middle- and backend-related changes made on the upstream repo
# to be applied to the corresponding new directories:
#
# Changes in the upstream "asmcomp" directory are applied to "backend" subdirectory.
# Changes in the upstream "middle_end" directory are applied to "middle_end" subdirectory.
# Other changes are apply to "ocaml" subdirectory.
#
# To apply the changes into ocaml/asmcomp and ocaml/middle-end as well, invoke
# this script with "-everywhere" command line flag.
#
# To apply the changes from flambda-backend's "ocaml/asmcomp" subdirectory
# to "backend" subdirectory, invoke this script with "-across" command line flag.
#
# To backport a change from upstream to ocaml/ directory only, simply invoke
# "git cherry-pick REVISION" from "ocaml" subdirectory.
# Alternatively, invoke this script with "-ocaml-only"
#
# If a patch does not apply cleanly, the script stops, and the rest of
# the patches need to be applied manually.  If "git am" or "git apply"
# fail, try "patch -p1 < file.patch" but be careful about "git add ." after it,
# because it generates .rej and .orig files that we do not want to commit by mistake.
#
# CR-someday gyorsh: there must be  a simpler way to do it.
# if not, this script is too long and should be rewritten in ocaml.
set -eu -o pipefail

usage () {
cat <<EOF
Usage:
  $0 REVISION [-everywhere|-ocaml-only|-across]
EOF
exit 1

}

everywhere=false
ocamlonly=false
across=false

if [ $# -eq 0 ]; then
    echo "ERROR: missing arguments"
    usage
elif [ $# -gt 2 ]; then
    echo "ERROR: too many arguments"
    usage
else
    while [ $# -ge 1 ]; do
        if [ "$1" = "-everywhere" ]; then
            everywhere=true
        elif [ "$1" = "-ocaml-only" ]; then
            ocamlonly=true
        elif [ "$1" = "-across" ]; then
            across=true
        else
            rev=$1
        fi
        shift
        continue
    done
fi

if [ -z "${rev+x}" ]; then
    echo "Missing REVISION"
    usage
fi

# check that git status is empty
if [[ -n $(git status -s) ]] ; then
    echo "Git status is not empty"
    exit 2
fi

dir="."
subtree="."
patches=()
format_patch () {

    temp=$(mktemp -d)
    mkdir -p "$temp"
    if [ "$dir" = "." ] ; then
        relative=""
    else
        relative="--relative=$dir"
    fi
    patchfile=$(git format-patch -p $relative \
                    --src-prefix=a/$subtree/ --dst-prefix=b/$subtree/ \
                    -o "$temp" "$rev" -1 -- "$files")
    if [ -s "$patchfile" ]; then
        # patch file is not empty
        echo "$patchfile"
        patches+=("$patchfile")
    else
        # patch file is empty, ignore it
        rm "$patchfile"
        rmdir "$temp"
    fi
}

apply_patches () {

    for index in "${!patches[@]}"; do
        patchfile="${patches[$index]}"
        echo "Applying $patchfile"
        if [ "$index" -eq 0 ] ; then
            # use the commit details of the first patch
            git am -3 "$patchfile" "--committer-date-is-author-date"
        else
            # for subsequent patches, just amend the previous commit
            git apply -3 "$patchfile" #"--whitespace=fix" ?
            git add .
            git commit --amend --no-edit
        fi
        temp=$(dirname "$patchfile")
        rm "$patchfile"
        rmdir "$temp"
    done
}

if [ "$ocamlonly" = true ] || [ "$everywhere" = true ] ; then
    subtree="ocaml" dir="." files=":!Changes :!boot" format_patch
    if [ "$everywhere" = true ] ; then
        subtree="backend" dir="asmcomp" files="." format_patch
        subtree="middle_end" dir="middle_end" files="." format_patch
    fi
elif [ "$across" = true ] ; then
     subtree="backend" dir="ocaml/asmcomp" files="." format_patch
     subtree="middle_end" dir="ocaml/middle_end" files="." format_patch
     dir="backend" subtree="ocaml/asmcomp" files="." format_patch
     dir="middle_end" subtree="ocaml/middle_end" files="." format_patch
else
    subtree="backend" dir="asmcomp" files="." format_patch
    subtree="middle_end" dir="middle_end" files="." format_patch
    subtree="ocaml" dir="." files=":!Changes :!boot :!asmcomp :!middle_end" format_patch
fi

apply_patches
