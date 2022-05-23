let () =
  exit (Optmaindriver.main Sys.argv Format.err_formatter
    ~flambda2:Flambda2.lambda_to_cmm)
