#include <caml/mlvalues.h>

/* returns a value that represents a number of words */
CAMLprim value caml_header_reserved_bits(value unit)
{
  return Val_long(HEADER_RESERVED_BITS) ;
}
