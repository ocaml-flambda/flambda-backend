#include <caml/mlvalues.h>
#include <stdbool.h>

CAMLextern void __real_caml_modify(value *fp, value v);

static bool called_modify = false;

void (*replace_caml_modify_hook)(void) = NULL;

CAMLprim void __wrap_caml_modify(value *fp, value v)
{
  called_modify = true;
  __real_caml_modify(fp, v);
  if (replace_caml_modify_hook != NULL) replace_caml_modify_hook ();
}

CAMLprim value replace_caml_modify_called_modify()
{
  return Val_bool(called_modify);
}

CAMLprim value replace_caml_modify_reset()
{
  called_modify = false;
  return Val_unit;
}

CAMLextern void __real_caml_modify_local(value obj, intnat i, value val);

CAMLprim void __wrap_caml_modify_local(value obj, intnat i, value val)
{
  __real_caml_modify_local(obj, i, val);
  called_modify = true;
  if (replace_caml_modify_hook != NULL) replace_caml_modify_hook ();
}
