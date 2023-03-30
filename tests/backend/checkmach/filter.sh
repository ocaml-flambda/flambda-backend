#!/bin/sh

sed -r 's/Error: Annotation check for zero_alloc( strict | )failed on function caml(.*)_[0-9]+(_[0-9]+_code)?$/Error: Annotation check for zero_alloc\1failed on function caml\2_HIDE_STAMP/'
