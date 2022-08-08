let () =
  exit (Optmaindriver.main (module Unix : Owee.Unix_intf.S) Sys.argv
    Format.err_formatter
    ~flambda2:Flambda2.lambda_to_cmm)
