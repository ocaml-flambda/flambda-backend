module DLL = Flambda_backend_utils.Doubly_linked_list

let cfg :
    Cfg_with_layout.t->
    Cfg_with_layout.t =
 fun cfg -> cfg

let dump ppf cfg_with_layout ~msg =
  let open Format in
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  fprintf ppf "\nvectorization extra information for %s\n" msg;
  fprintf ppf "%s\n" (Cfg.fun_name cfg);
  fprintf ppf "layout.length=%d\n" (Cfg_with_layout.layout cfg_with_layout |> DLL.length);
  let block_count = (Label.Tbl.length cfg.blocks) in
  fprintf ppf "blocks.length=%d\n" block_count;
  let body_instruction_count =
    Cfg.fold_body_instructions cfg ~f:(fun sum _ -> sum + 1) ~init:0 in
  fprintf ppf "body instruction count=%d\n" body_instruction_count;
  fprintf ppf "terminator instruction count=%d\n" block_count;
  fprintf ppf "body and terminator instruction count=%d\n" (body_instruction_count + block_count)
