let flambda2_backend =
  (module Flambda2_backend_impl : Flambda2__Flambda_backend_intf.S)

let () =
  (match Sys.backend_type with
   | Native -> Memtrace.trace_if_requested ~context:"ocamlopt" ()
   | Bytecode | Other _ -> ());
  exit (Optmaindriver.main Sys.argv Format.err_formatter
    ~flambda2_backend ~flambda2_to_cmm:Flambda2_to_cmm.To_cmm.unit)
