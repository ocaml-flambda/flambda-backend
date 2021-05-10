let () =
  Memtrace.trace_if_requested ();
  exit (Optmaindriver.main Sys.argv Format.err_formatter)
