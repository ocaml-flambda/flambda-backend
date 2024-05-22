type debug_thing =
  | Debug_ocamldebug
  | Debug_js_of_ocaml
  | Debug_subprocs
  | Debug_backtraces
  | Debug_bounds_checking
  | Debug_disable_bytecode_opt
  | Debug_dwarf_cfi
  | Debug_dwarf_loc
  | Debug_dwarf_functions
  | Debug_dwarf_scopes
  | Debug_dwarf_vars
  | Debug_dwarf_call_sites
  | Debug_dwarf_cmm

let bytecode_g =
  [ Debug_ocamldebug;
    Debug_js_of_ocaml;
    Debug_subprocs;
    Debug_backtraces;
    Debug_disable_bytecode_opt ]

let g0 = []

let g1 =
  [ Debug_subprocs;
    Debug_backtraces;
    Debug_bounds_checking;
    Debug_disable_bytecode_opt;
    Debug_dwarf_cfi;
    Debug_dwarf_loc ]

let g2 =
  [ Debug_subprocs;
    Debug_backtraces;
    Debug_bounds_checking;
    Debug_disable_bytecode_opt;
    Debug_dwarf_cfi;
    Debug_dwarf_loc;
    Debug_dwarf_functions ]

let g3 =
  [ Debug_subprocs;
    Debug_backtraces;
    Debug_bounds_checking;
    Debug_disable_bytecode_opt;
    Debug_dwarf_cfi;
    Debug_dwarf_loc;
    Debug_dwarf_functions;
    Debug_dwarf_scopes;
    Debug_dwarf_vars;
    Debug_dwarf_call_sites;
    Debug_dwarf_cmm ]

let all_g_levels = ["g0", g0; "g1", g1; "g2", g2; "g3", g3]

let current_debug_settings = ref g0

let use_g0 () = current_debug_settings := g0

let use_g1 () = current_debug_settings := g1

let use_g2 () = current_debug_settings := g2

let use_g3 () =
  Clflags.binary_annotations := true;
  (* since [Debug_dwarf_vars] is present *)
  current_debug_settings := g3

let use_g () =
  if !Clflags.native_code
  then use_g1 ()
  else current_debug_settings := bytecode_g

let restrict_to_upstream_dwarf = ref true

(* Currently the maximum number of stack slots, see asmgen.ml *)
let dwarf_max_function_complexity = ref 50

let dwarf_for_startup_file = ref false

let debug_thing thing = List.mem thing !current_debug_settings

let set_debug_thing thing =
  let new_settings =
    List.filter (fun thing' -> thing <> thing') !current_debug_settings
  in
  current_debug_settings := thing :: new_settings

let clear_debug_thing thing =
  let new_settings =
    List.filter (fun thing' -> thing <> thing') !current_debug_settings
  in
  current_debug_settings := new_settings

let describe_debug_default_internal ~negate thing =
  let defaults =
    List.filter_map
      (fun (level, things_enabled_at_level) ->
        let enabled_at_level = List.mem thing things_enabled_at_level in
        let state = if negate then not enabled_at_level else enabled_at_level in
        if state then Some ("-" ^ level) else None)
      all_g_levels
  in
  match defaults with
  | [] -> ""
  | defaults -> " (default with " ^ String.concat ", " defaults ^ ")"

let describe_debug_default thing =
  describe_debug_default_internal ~negate:false thing

let describe_debug_default_negated thing =
  describe_debug_default_internal ~negate:true thing

type dwarf_version =
  | Four
  | Five

let default_gdwarf_version = Four

let gdwarf_version = ref default_gdwarf_version

let default_gdwarf_offsets = false

let gdwarf_offsets = ref default_gdwarf_offsets

let default_ddebug_invariants = false

let ddebug_invariants = ref default_ddebug_invariants

type dwarf_format =
  | Thirty_two
  | Sixty_four

let default_gdwarf_format = Thirty_two

let gdwarf_format = ref default_gdwarf_format

let default_gdwarf_self_tail_calls = true

let gdwarf_self_tail_calls = ref default_gdwarf_self_tail_calls

let gdwarf_may_alter_codegen = ref false

let dwarf_inlined_frames = ref false
