#!/bin/sh

# CR ocaml 5 all-runtime5: remove __ mangling once we're always using the 5 runtime
sed -r 's/Error: Annotation check for zero_alloc( strict | )failed on function ([^ ]*) \(caml(.*)_[0-9]+(_[0-9]+_code)?\)$/Error: Annotation check for zero_alloc\1failed on function \2 \(caml\3_HIDE_STAMP\)/' | \
    sed -r 's/  direct (tail)?call caml(.*)_[0-9]+(_[0-9]+_code)?( on a path to .*)?$/  direct \1call caml\2_HIDE_STAMP\4/' | sed -r 's/  probe (test)? handler caml(.*)_[0-9]+(_[0-9]+_code)?( on a path to .*)?$/  probe \1 handler caml\2_HIDE_STAMP\4/' | \
    sed 's/__/./'
