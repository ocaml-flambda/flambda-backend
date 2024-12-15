/* This C file is part of the [array_element_size_in_bytes.ml] test. See comment
   there. It should eventually be deleted. */
#include "caml/mlvalues.h"

value custom_block_padding_native(value v) {
  return(Val_long(1));
}

value custom_block_padding_byte(value v) {
  return(Val_long(0));
}
