let () =
  let cmi = Cmi_format.read_cmi "foo.cmi" in
  let data = Marshal.to_string
    ( cmi.cmi_name,
      Subst.Lazy.force_signature cmi.cmi_sign,
      cmi.cmi_crcs,
      cmi.cmi_flags )
    []
  in
  let filename = Sys.argv.(1) in
  let oc = open_out filename in
  Printf.fprintf oc "let foo = %S\n" data;
  close_out oc
