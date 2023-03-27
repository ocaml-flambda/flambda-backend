#!/bin/sh
# This location filter is normalizing backtraces so that
# closure and flambda backtrace can be compared more easily
# It runs two transformations:
# - remove "(inlined)" annotations
# - remove "by primitive operations" because flambda2 handles
#   array primitives a bit differently and thus does not emit
#   this "by primitive operation" for e.g. array accesses
sed -e 's/ (inlined)//' -e 's/ by primitive operation//'
