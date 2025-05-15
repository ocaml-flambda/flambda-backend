(* This program simply runs a register allocator on the all the functions saved
   in a .cmir-cfg-regalloc file. *)

module List = ListLabels

type register_allocator =
  | GI
  | IRC
  | LS

let allocators = ["gi", GI; "irc", IRC; "ls", LS]

external time_include_children : bool -> float
  = "caml_sys_time_include_children"

let cpu_time () = time_include_children false

let process_function
    (register_allocator : register_allocator)
    (cfg_with_layout : Cfg_with_layout.t) (cmm_label : Label.t)
    (reg_stamp : int) (relocatable_regs : Reg.t list) =
  Printf.eprintf "  processing function %S...\n%!"
    (Cfg_with_layout.cfg cfg_with_layout).fun_name;
  Printf.eprintf "    %d register(s)...\n%!" (List.length relocatable_regs);
  Cmm.reset ();
  Cmm.set_label cmm_label;
  Reg.For_testing.set_state ~stamp:reg_stamp ~relocatable_regs;
  let cfg_with_infos = Cfg_with_infos.make cfg_with_layout in
  let start_time = cpu_time () in
  let (_ : Cfg_with_infos.t) =
    match register_allocator with
    | GI -> Regalloc_gi.run cfg_with_infos
    | IRC -> Regalloc_irc.run cfg_with_infos
    | LS -> Regalloc_ls.run cfg_with_infos
  in
  let end_time = cpu_time () in
  Printf.eprintf "  register allocation took %gs...\n%!" (end_time -. start_time);
  ()

let process_file (file : string) (register_allocator : register_allocator) =
  Printf.eprintf "processing file %S...\n%!" file;
  let unit_info, _digest = Cfg_format.restore file in
  List.iter unit_info.items ~f:(fun (item : Cfg_format.cfg_item_info) ->
      begin
        match item with
        | Cfg _ -> ()
        | Data _ -> ()
        | Cfg_before_regalloc
            { cfg_with_layout; cmm_label; reg_stamp; relocatable_regs } ->
          process_function register_allocator cfg_with_layout cmm_label
            reg_stamp relocatable_regs
      end)

let () =
  let register_allocator = ref None in
  let set_register_allocator str =
    match List.assoc_opt str allocators with
    | None -> assert false
    | Some allocator -> register_allocator := Some allocator
  in
  let files = ref [] in
  let args : (Arg.key * Arg.spec * Arg.doc) list =
    [ ( "-regalloc",
        Arg.Symbol (List.map allocators ~f:fst, set_register_allocator),
        "  Choose register allocator" ) ]
  in
  let anonymous file = files := file :: !files in
  Arg.parse args anonymous
    "run register allocation on a .cmir-cfg-regalloc file";
  match !register_allocator with
  | None ->
    Printf.eprintf
      "*** error: register allocator was not set (use -regalloc)\n%!";
    exit 1
  | Some register_allocator ->
    List.iter (List.rev !files) ~f:(fun file ->
        process_file file register_allocator)
