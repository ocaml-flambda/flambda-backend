#!/bin/bash

sed -r 's/Error: Annotation check for noalloc_strict failed on function caml(.*)_[0-9]+(_[0-9]+_code)?$/Error: Annotation check for noalloc_strict failed on function caml\1_HIDE_STAMP/' | sed -r 's/Error: Annotation check for noalloc failed on function caml(.*)_[0-9]+(_[0-9]+_code)?$/Error: Annotation check for noalloc failed on function caml\1_HIDE_STAMP/'
