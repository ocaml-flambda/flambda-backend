#!/bin/sh

sed -r 's/Error: Annotation check for zero_alloc( strict | )failed on function ([^ ]*) \(caml(.*)_[0-9]+(_[0-9]+_code)?\)$/Error: Annotation check for zero_alloc\1failed on function \2 \(caml\3_HIDE_STAMP\)/'
