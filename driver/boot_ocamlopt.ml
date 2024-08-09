let () =
  exit (Optmaindriver.main (Obj.magic 0) (*(module Unix : Compiler_owee.Unix_intf.S) XXX *) Sys.argv
    Format.err_formatter
    ~flambda2:Flambda2.lambda_to_cmm)
