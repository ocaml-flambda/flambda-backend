let () =
  print_endline "(";
  for i = 1 to Array.length Sys.argv - 1 do
    let path = Sys.argv.(i) in
    let basename = Filename.basename path in
    let prefix = Filename.chop_extension basename in
    let extension = Filename.extension basename in
    let tgt_prefix_opt =
      match prefix, extension with
      | "boot_ocamlopt", ".mli"
      | "flambda_backend_main", ".mli"
      | "flambda_backend_main_native", ".mli"
      | "internal_assembler", ".mli"
      | "mach_checks", ".mli"
      | "relocation_table", ".mli"
      | "symbol_entry", ".mli"
      | "symbol_utils", ".mli"
      | "optmaindriver", ".cmx"
      | "relocation_table", (".cmt" | ".cmi" | ".cmti" | ".cmx")
      | "symbol_entry", (".cmt" | ".cmi" | ".cmti" | ".cmx") ->
        None
      | "zero_alloc_checker", ".mli" -> Some "mach_checks"
      | "cSE", (".cmi" | ".cmt" | ".cmx") -> Some "CSE"
      | "cSEgen", (".cmi" | ".cmt" | ".cmti" | ".cmx") -> Some "CSEgen"
      | _, _ -> Some prefix
    in
    let tgt_dir_prefix =
      if String.starts_with ~prefix:"external/" path
      then ""
      else "compiler-libs/"
    in
    match tgt_prefix_opt with
    | None -> ()
    | Some tgt_prefix ->
      Printf.printf "(%s as %s%s%s)\n" path tgt_dir_prefix tgt_prefix extension
  done;
  print_endline ")"
