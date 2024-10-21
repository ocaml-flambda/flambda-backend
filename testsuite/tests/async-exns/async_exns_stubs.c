#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <assert.h>

static value sys_break(void)
{
  const value *break_exn;
  break_exn = caml_named_value("Sys.Break");
  if (break_exn == NULL) abort ();
  return *break_exn;
}

value test_caml_callback_exn_collects_async_exns(value raise_break_async)
{
  value res = caml_callback_exn(raise_break_async, Val_unit);
  assert(Is_exception_result(res));
  assert(Extract_exception(res) == sys_break());
  return Val_unit;
}

value test_caml_callback2_exn_collects_async_exns(value raise_break_async)
{
  value res = caml_callback2_exn(raise_break_async, Val_unit, Val_unit);
  assert(Is_exception_result(res));
  assert(Extract_exception(res) == sys_break());
  return Val_unit;
}

value test_caml_callback3_exn_collects_async_exns(value raise_break_async)
{
  value res;
  res = caml_callback3_exn(raise_break_async, Val_unit, Val_unit, Val_unit);
  assert(Is_exception_result(res));
  assert(Extract_exception(res) == sys_break());
  return Val_unit;
}

value test_caml_callbackN_exn_collects_async_exns(value raise_break_async)
{
  value res;
  value args[] = { Val_unit, Val_unit, Val_unit, Val_unit };
  res = caml_callbackN_exn(raise_break_async, 4, args);
  assert(Is_exception_result(res));
  assert(Extract_exception(res) == sys_break());
  return Val_unit;
}

value invoke_caml_callback(value raise_break_async)
{
  return caml_callback(raise_break_async, Val_unit);
}

value invoke_caml_callback2(value raise_break_async)
{
  return caml_callback2(raise_break_async, Val_unit, Val_unit);
}

value invoke_caml_callback3(value raise_break_async)
{
  return caml_callback3(raise_break_async, Val_unit, Val_unit, Val_unit);
}

value invoke_caml_callbackN(value raise_break_async)
{
  value args[] = { Val_unit, Val_unit, Val_unit, Val_unit };
  return caml_callbackN(raise_break_async, 4, args);
}
