#include <caml/mlvalues.h>
#include <stdbool.h>

// Use this to track whether caml_modify has been called
static bool called_modify = false;

CAMLprim value replace_caml_modify_called_modify()
{
  return Val_bool(called_modify);
}

CAMLprim value replace_caml_modify_reset()
{
  called_modify = false;
  return Val_unit;
}

// This is a reference to the original caml_modify
CAMLextern void __real_caml_modify(value *fp, value v);

// This is called instead of caml_modify
CAMLprim void __wrap_caml_modify(value *fp, value v)
{
  // Record that caml_modify was called and then call the actual caml_modify
  called_modify = true;
  __real_caml_modify(fp, v);
}

// Treat caml_modify_local the same was as caml_modify

CAMLextern void __real_caml_modify_local(value obj, intnat i, value val);

CAMLprim void __wrap_caml_modify_local(value obj, intnat i, value val)
{
  __real_caml_modify_local(obj, i, val);
  called_modify = true;
}
