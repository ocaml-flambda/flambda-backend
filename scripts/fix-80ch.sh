#!/usr/bin/env bash

# fix-80ch.sh: Run 80ch.sh and open the list of long lines in your editor
#              (VSCode not supported)

# A lot of this code was copied from git-jump
# (see https://github.com/mykolaharmash/git-jump/blob/main/LICENSE)

open_editor() {
  case "$EDITOR" in
  *emacs*)
    # Supported editor values are:
    # - emacs
    # - emacsclient
    # - emacsclient -t
    #
    # Wait for completion of the asynchronously executed process
    # to avoid race conditions in case of "emacsclient".
    eval "$EDITOR --eval \"(let ((buf (grep \\\"cat \$1\\\"))) (pop-to-buffer buf) (select-frame-set-input-focus (selected-frame)) (while (get-buffer-process buf) (sleep-for 0.1)))\""
    ;;
  *)
    # assume anything else is vi-compatible
    eval "$EDITOR -q \$1"
    ;;
  esac
}

trap 'rm -f "$tmp"' 0 1 2 3 15
tmp=`mktemp -t fix-80ch.XXXXXX` || exit 1
CH80_FORMAT=fix "$(dirname "$0")"/80ch.sh >"$tmp"
test -s "$tmp" || exit 0
open_editor "$tmp"
