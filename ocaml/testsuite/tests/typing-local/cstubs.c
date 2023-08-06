#include "caml/mlvalues.h"
#include "caml/gc.h"
#include "caml/memory.h"

value make_dumb_external_block(value unit)
{
    value *p = caml_stat_alloc(sizeof(value));
    *p = Make_header(0, 0, Caml_black);
    return Val_int(((intnat)p) + 1);
}