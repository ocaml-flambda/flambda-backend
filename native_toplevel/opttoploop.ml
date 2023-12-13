(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The interactive toplevel loop *)

open Format
open Config
open Misc
open Parsetree
open Types
open Typedtree
open Outcometree
open Ast_helper

module Genprintval = Genprintval_native

let any_flambda = Config.flambda || Config.flambda2

type res = Ok of Obj.t | Err of string
type evaluation_outcome = Result of Obj.t | Exception of exn

let _dummy = (Ok (Obj.magic 0), Err "")

module Jit = struct
  type t =
    {
      load : Format.formatter -> Lambda.program -> evaluation_outcome;
      lookup_symbol : string -> Obj.t option;
    }
end

let jit = ref None

let register_jit j = jit := Some j

external ndl_run_toplevel: string -> string -> res
  = "caml_natdynlink_run_toplevel"

let default_lookup sym =
  Dynlink.unsafe_get_global_value ~bytecode_or_asm_symbol:sym

let global_symbol comp_unit =
  let lookup =
    match !jit with
      | None -> default_lookup
      | Some {Jit.lookup_symbol; _} -> lookup_symbol
  in
  let linkage_name =
    Symbol.for_compilation_unit comp_unit
    |> Symbol.linkage_name
    |> Linkage_name.to_string
  in
  match lookup linkage_name with
  | None ->
    fatal_error ("Opttoploop.global_symbol " ^
      (Compilation_unit.full_path_as_string comp_unit))
  | Some obj -> obj

let need_symbol sym =
  Option.is_none (Dynlink.unsafe_get_global_value ~bytecode_or_asm_symbol:sym)

let dll_run dll entry =
  match (try Result (Obj.magic (ndl_run_toplevel dll entry))
         with exn -> Exception exn)
  with
    | Exception _ as r -> r
    | Result r ->
        match Obj.magic r with
          | Ok x -> Result x
          | Err s -> fatal_error ("Opttoploop.dll_run " ^ s)


type directive_fun =
   | Directive_none of (unit -> unit)
   | Directive_string of (string -> unit)
   | Directive_int of (int -> unit)
   | Directive_ident of (Longident.t -> unit)
   | Directive_bool of (bool -> unit)

type directive_info = {
  section: string;
  doc: string;
}

let remembered = ref Ident.empty

let rec remember phrase_name i = function
  | [] -> ()
  | Sig_module (_, _, { md_type = Mty_alias _; _ }, _, _)
    :: rest ->
      remember phrase_name i rest
  | Sig_value  (id, _, _) :: rest
  | Sig_module (id, _, _, _, _) :: rest
  | Sig_typext (id, _, _, _) :: rest
  | Sig_class  (id, _, _, _) :: rest ->
      remembered := Ident.add id (phrase_name, i) !remembered;
      remember phrase_name (succ i) rest
  | _ :: rest -> remember phrase_name i rest

let toplevel_value id =
  try Ident.find_same id !remembered
  with _ -> failwith ("Unknown ident: " ^ Ident.unique_name id)

let close_phrase lam =
  let open Lambda in
  Ident.Set.fold (fun id l ->
    let glb, pos = toplevel_value id in
    let glob =
      Lprim (mod_field pos,
             [Lprim (Pgetglobal glb, [], Loc_unknown)],
             Loc_unknown)
    in
    Llet(Strict, Lambda.layout_module_field, id, glob, l)
  ) (free_variables lam) lam

let toplevel_value id =
  let glob, pos =
    if any_flambda then toplevel_value id else Translmod.nat_toplevel_name id
  in
  (Obj.magic (global_symbol glob)).(pos)

(* Return the value referred to by a path *)

let rec eval_address = function
  | Env.Aunit cu ->
      global_symbol cu
  | Env.Alocal id ->
      toplevel_value id
  | Env.Adot(a, pos) ->
      Obj.field (eval_address a) pos

let eval_path find env path =
  match find path env with
  | addr -> eval_address addr
  | exception Not_found ->
      fatal_error ("Cannot find address for: " ^ (Path.name path))

let eval_module_path env path =
  eval_path Env.find_module_address env path

let eval_value_path env path =
  eval_path Env.find_value_address env path

let eval_extension_path env path =
  eval_path Env.find_constructor_address env path

let eval_class_path env path =
  eval_path Env.find_class_address env path

(* To print values *)

module EvalPath = struct
  type valu = Obj.t
  exception Error
  let eval_address addr =
    try eval_address addr with _ -> raise Error
  let same_value v1 v2 = (v1 == v2)
end

module Printer = Genprintval.Make(Obj)(EvalPath)

let max_printer_depth = ref 100
let max_printer_steps = ref 300

let print_out_value = Oprint.out_value
let print_out_type = Oprint.out_type
let print_out_class_type = Oprint.out_class_type
let print_out_module_type = Oprint.out_module_type
let print_out_type_extension = Oprint.out_type_extension
let print_out_sig_item = Oprint.out_sig_item
let print_out_signature = Oprint.out_signature
let print_out_phrase = Oprint.out_phrase

let print_untyped_exception ppf obj =
  !print_out_value ppf (Printer.outval_of_untyped_exception obj)
let outval_of_value env obj ty =
  Printer.outval_of_value !max_printer_steps !max_printer_depth
    (fun _ _ _ -> None) env obj ty
let print_value env obj ppf ty =
  !print_out_value ppf (outval_of_value env obj ty)

type ('a, 'b) gen_printer = ('a, 'b) Genprintval.gen_printer =
  | Zero of 'b
  | Succ of ('a -> ('a, 'b) gen_printer)

let install_printer = Printer.install_printer
let install_generic_printer = Printer.install_generic_printer
let install_generic_printer' = Printer.install_generic_printer'
let remove_printer = Printer.remove_printer

(* Hooks for parsing functions *)

let parse_toplevel_phrase = ref Parse.toplevel_phrase
let parse_use_file = ref Parse.use_file
let print_location = Location.print_loc
let print_error = Location.print_report
let print_warning = Location.print_warning
let input_name = Location.input_name

let parse_mod_use_file name lb =
  let modname =
    String.capitalize_ascii
      (Filename.remove_extension (Filename.basename name))
  in
  let items =
    List.concat
      (List.map
         (function Ptop_def s -> s | Ptop_dir _ -> [])
         (!parse_use_file lb))
  in
  [ Ptop_def
      [ Str.module_
          (Mb.mk
             (Location.mknoloc (Some modname))
             (Mod.structure items)
          )
       ]
   ]

(* Hook for initialization *)

let toplevel_startup_hook = ref (fun () -> ())

type event = ..
type event +=
  | Startup
  | After_setup

let hooks = ref []

let add_hook f = hooks := f :: !hooks

let () =
  add_hook (function
      | Startup -> !toplevel_startup_hook ()
      | _ -> ())

let run_hooks hook = List.iter (fun f -> f hook) !hooks

(* Load in-core and execute a lambda term *)

let phrase_seqid = ref 0
let phrase_name = ref "TOP"

(* CR-soon trefis for mshinwell: copy/pasted from Optmain. Should it be shared
   or?
   mshinwell: It should be shared, but after 4.03. *)
module Backend = struct
  (* See backend_intf.mli. *)

  let really_import_approx = Import_approx.really_import_approx
  let import_symbol = Import_approx.import_symbol

  let size_int = Arch.size_int
  let big_endian = Arch.big_endian

  let max_sensible_number_of_arguments =
    (* The "-1" is to allow for a potential closure environment parameter. *)
    Proc.max_arguments_for_tailcalls - 1
end
let backend = (module Backend : Backend_intf.S)

let default_load ppf (program : Lambda.program) =
  let dll =
    if !Clflags.keep_asm_file then !phrase_name ^ ext_dll
    else Filename.temp_file ("caml" ^ !phrase_name) ext_dll
  in
  let filename = Filename.chop_extension dll in
  let pipeline : Asmgen.pipeline =
    if Config.flambda2 then
      Direct_to_cmm (Flambda2.lambda_to_cmm ~keep_symbol_tables:true)
    else
      let middle_end =
        if Config.flambda then Flambda_middle_end.lambda_to_clambda
        else Closure_middle_end.lambda_to_clambda
      in
      Via_clambda { middle_end; backend }
  in
  Asmgen.compile_implementation
    (module Unix : Compiler_owee.Unix_intf.S)
    ~toplevel:need_symbol
    ~filename ~prefixname:filename
    ~pipeline ~ppf_dump:ppf
    program;
  Asmlink.call_linker_shared ~native_toplevel:true [filename ^ ext_obj] dll;
  Sys.remove (filename ^ ext_obj);
  let dll =
    if Filename.is_implicit dll
    then Filename.concat (Sys.getcwd ()) dll
    else dll in
  (* CR-someday lmaurer: The manual prefixing here feels wrong. Probably
     [!phrase_name] should be a [Compilation_unit.t] (from which we can extract
     a linkage name like civilized folk). That will be easier to do once we have
     better types in, say, the [Translmod] API. *)
  let res = dll_run dll ("caml" ^ !phrase_name) in
  (try Sys.remove dll with Sys_error _ -> ());
  (* note: under windows, cannot remove a loaded dll
     (should remember the handles, close them in at_exit, and then remove
     files) *)
  res

let load_lambda ppf ~compilation_unit ~required_globals lam size =
  if !Clflags.dump_rawlambda then fprintf ppf "%a@." Printlambda.lambda lam;
  let slam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then fprintf ppf "%a@." Printlambda.lambda slam;
  let program =
    { Lambda.
      code = slam;
      main_module_block_size = size;
      arg_block_field = None;
      compilation_unit;
      required_globals;
    }
  in
  match !jit with
  | None -> default_load ppf program
  | Some {Jit.load; _} -> load ppf program

(* Print the outcome of an evaluation *)

let pr_item =
  Printtyp.print_items
    (fun env -> function
       | Sig_value(id, {val_kind = Val_reg; val_type; _}, _) ->
          Some (outval_of_value env (toplevel_value id) val_type)
      | _ -> None
    )

(* The current typing environment for the toplevel *)

let toplevel_env = ref Env.empty
let toplevel_sig = ref []

(* Print an exception produced by an evaluation *)

let print_out_exception ppf exn outv =
  !print_out_phrase ppf (Ophr_exception (exn, outv))

let print_exception_outcome ppf exn =
  if exn = Out_of_memory then Gc.full_major ();
  let outv = outval_of_value !toplevel_env (Obj.repr exn) Predef.type_exn in
  print_out_exception ppf exn outv

(* The table of toplevel directives.
   Filled by functions from module topdirs. *)

let directive_table = (Hashtbl.create 23 : (string, directive_fun) Hashtbl.t)

let directive_info_table =
  (Hashtbl.create 23 : (string, directive_info) Hashtbl.t)

let add_directive name dir_fun dir_info =
  Hashtbl.add directive_table name dir_fun;
  Hashtbl.add directive_info_table name dir_info

(* Give a name to an unnamed expression *)

let name_expression ~loc ~attrs sort exp =
  let name = "_$" in
  let id = Ident.create_local name in
  let vd =
    { val_type = exp.exp_type;
      val_kind = Val_reg;
      val_loc = loc;
      val_attributes = attrs;
      val_uid = Uid.internal_not_actually_unique; }
  in
  let sg = [Sig_value(id, vd, Exported)] in
  let pat =
    { pat_desc = Tpat_var(id, mknoloc name, vd.val_uid, Mode.Value.legacy);
      pat_loc = loc;
      pat_extra = [];
      pat_type = exp.exp_type;
      pat_env = exp.exp_env;
      pat_attributes = []; }
  in
  let vb =
    { vb_pat = pat;
      vb_expr = exp;
      vb_attributes = attrs;
      vb_loc = loc;
      vb_sort = sort }
  in
  let item =
    { str_desc = Tstr_value(Nonrecursive, [vb]);
      str_loc = loc;
      str_env = exp.exp_env; }
  in
  let final_env = Env.add_value id vd exp.exp_env in
  let str =
    { str_items = [item];
      str_type = sg;
      str_final_env = final_env }
  in
  str, sg

(* Execute a toplevel phrase *)

let execute_phrase print_outcome ppf phr =
  match phr with
  | Ptop_def sstr ->
      let oldenv = !toplevel_env in
      let oldsig = !toplevel_sig in
      incr phrase_seqid;
      phrase_name := Printf.sprintf "TOP%i" !phrase_seqid;
      let compilation_unit =
        Compilation_unit.create Compilation_unit.Prefix.empty
          (!phrase_name |> Compilation_unit.Name.of_string)
      in
      Compilenv.reset compilation_unit;
      Typecore.reset_delayed_checks ();
      let (str, sg, names, _shape, newenv) =
        Typemod.type_toplevel_phrase oldenv oldsig sstr
      in
      if !Clflags.dump_typedtree then Printtyped.implementation ppf str;
      let sg' = Typemod.Signature_names.simplify newenv names sg in
      let coercion = Includemod.signatures oldenv ~mark:Mark_positive sg sg' in
      Typecore.force_delayed_checks ();
      let str, sg', rewritten =
        match str.str_items with
        | [ { str_desc = Tstr_eval (e, sort, attrs) ; str_loc = loc } ]
        | [ { str_desc = Tstr_value (Asttypes.Nonrecursive,
                                      [{ vb_expr = e
                                       ; vb_pat =
                                           { pat_desc = Tpat_any;
                                             _ }
                                       ; vb_attributes = attrs
                                       ; vb_sort = sort }])
            ; str_loc = loc }
          ] ->
            let str, sg' = name_expression ~loc ~attrs sort e in
            str, sg', true
        | _ -> str, sg', false
      in
      let compilation_unit, res, required_globals, size =
        if any_flambda then
          let { Lambda.compilation_unit; main_module_block_size = size;
                required_globals; code = res } =
            Translmod.transl_implementation compilation_unit
              (str, coercion, None)
              ~style:Plain_block
          in
          remember compilation_unit 0 sg';
          compilation_unit, close_phrase res, required_globals, size
        else
          let size, res = Translmod.transl_store_phrases compilation_unit str in
          compilation_unit, res, Compilation_unit.Set.empty, size
      in
      Warnings.check_fatal ();
      begin try
        toplevel_env := newenv;
        toplevel_sig := List.rev_append sg' oldsig;
        let res =
          load_lambda ppf ~required_globals ~compilation_unit res size
        in
        let out_phr =
          match res with
          | Result _ ->
              if any_flambda then
                (* CR-someday trefis: *)
                Env.register_import_as_opaque
                  (Compilation_unit.name compilation_unit)
              else
                Compilenv.record_global_approx_toplevel ();
              if print_outcome then
                Printtyp.wrap_printing_env ~error:false oldenv (fun () ->
                match str.str_items with
                | [] -> Ophr_signature []
                | _ ->
                    if rewritten then
                      match sg' with
                      | [ Sig_value (id, vd, _) ] ->
                          let outv =
                            outval_of_value newenv (toplevel_value id)
                              vd.val_type
                          in
                          let ty = Printtyp.tree_of_type_scheme vd.val_type in
                          Ophr_eval (outv, ty)
                      | _ -> assert false
                    else
                      Ophr_signature (pr_item oldenv sg'))
              else Ophr_signature []
          | Exception exn ->
              toplevel_env := oldenv;
              toplevel_sig := oldsig;
              if exn = Out_of_memory then Gc.full_major();
              let outv =
                outval_of_value !toplevel_env (Obj.repr exn) Predef.type_exn
              in
              Ophr_exception (exn, outv)
        in
        !print_out_phrase ppf out_phr;
        begin match out_phr with
        | Ophr_eval (_, _) | Ophr_signature _ -> true
        | Ophr_exception _ -> false
        end
      with x ->
        toplevel_env := oldenv; toplevel_sig := oldsig; raise x
      end
  | Ptop_dir {pdir_name = {Location.txt = dir_name; _}; pdir_arg; _ } ->
      let d =
        try Some (Hashtbl.find directive_table dir_name)
        with Not_found -> None
      in
      begin match d with
      | None ->
          fprintf ppf "Unknown directive `%s'.@." dir_name;
          false
      | Some d ->
          match d, pdir_arg with
          | Directive_none f, None -> f (); true
          | Directive_string f, Some {pdira_desc = Pdir_string s; _} -> f s; true
          | Directive_int f, Some {pdira_desc = Pdir_int (n,None); _} ->
             begin match Int_literal_converter.int n with
             | n -> f n; true
             | exception _ ->
               fprintf ppf "Integer literal exceeds the range of \
                            representable integers for directive `%s'.@."
                       dir_name;
               false
             end
          | Directive_int _, Some {pdira_desc = Pdir_int (_, Some _); _} ->
              fprintf ppf "Wrong integer literal for directive `%s'.@."
                dir_name;
              false
          | Directive_ident f, Some {pdira_desc = Pdir_ident lid; _} -> f lid; true
          | Directive_bool f, Some {pdira_desc = Pdir_bool b; _} -> f b; true
          | _ ->
              fprintf ppf "Wrong type of argument for directive `%s'.@."
                dir_name;
              false
      end

(* Read and execute commands from a file, or from stdin if [name] is "". *)

let use_print_results = ref true

let preprocess_phrase ppf phr =
  let phr =
    match phr with
    | Ptop_def str ->
        let str =
          Pparse.apply_rewriters_str ~restore:true ~tool_name:"ocaml" str
        in
        Ptop_def str
    | phr -> phr
  in
  if !Clflags.dump_parsetree then Printast.top_phrase ppf phr;
  if !Clflags.dump_source then Pprintast.top_phrase ppf phr;
  phr

let use_channel ppf ~wrap_in_module ic name filename =
  let lb = Lexing.from_channel ic in
  Location.init lb filename;
  (* Skip initial #! line if any *)
  Lexer.skip_hash_bang lb;
  let success =
    protect_refs [ R (Location.input_name, filename) ] (fun () ->
      try
        List.iter
          (fun ph ->
            let ph = preprocess_phrase ppf ph in
            if not (execute_phrase !use_print_results ppf ph) then raise Exit)
          (if wrap_in_module then
             parse_mod_use_file name lb
           else
             !parse_use_file lb);
        true
      with
      | Exit -> false
      | Sys.Break -> fprintf ppf "Interrupted.@."; false
      | x -> Location.report_exception ppf x; false) in
  success

let use_output ppf command =
  let fn = Filename.temp_file "ocaml" "_toploop.ml" in
  Misc.try_finally ~always:(fun () ->
      try Sys.remove fn with Sys_error _ -> ())
    (fun () ->
       match
         Printf.ksprintf Sys.command "%s > %s"
           command
           (Filename.quote fn)
       with
       | 0 ->
         let ic = open_in_bin fn in
         Misc.try_finally ~always:(fun () -> close_in ic)
           (fun () ->
              use_channel ppf ~wrap_in_module:false ic "" "(command-output)")
       | n ->
         fprintf ppf "Command exited with code %d.@." n;
         false)

let use_file ppf ~wrap_in_module name =
  match name with
  | "" ->
    use_channel ppf ~wrap_in_module stdin name "(stdin)"
  | _ ->
    match Load_path.find name with
    | filename ->
      let ic = open_in_bin filename in
      Misc.try_finally ~always:(fun () -> close_in ic)
        (fun () -> use_channel ppf ~wrap_in_module ic name filename)
    | exception Not_found ->
      fprintf ppf "Cannot find file %s.@." name;
      false

let mod_use_file ppf name =
  use_file ppf ~wrap_in_module:true name
let use_file ppf name =
  use_file ppf ~wrap_in_module:false name

let use_silently ppf name =
  protect_refs [ R (use_print_results, false) ] (fun () -> use_file ppf name)

(* Reading function for interactive use *)

let first_line = ref true
let got_eof = ref false;;

let read_input_default prompt buffer len =
  output_string stdout prompt; flush stdout;
  let i = ref 0 in
  try
    while true do
      if !i >= len then raise Exit;
      let c = input_char stdin in
      Bytes.set buffer !i c;
      incr i;
      if c = '\n' then raise Exit;
    done;
    (!i, false)
  with
  | End_of_file ->
      (!i, true)
  | Exit ->
      (!i, false)

let read_interactive_input = ref read_input_default

let refill_lexbuf buffer len =
  if !got_eof then (got_eof := false; 0) else begin
    let prompt =
      if !Clflags.noprompt then ""
      else if !first_line then "# "
      else if !Clflags.nopromptcont then ""
      else if Lexer.in_comment () then "* "
      else "  "
    in
    first_line := false;
    let (len, eof) = !read_interactive_input prompt buffer len in
    if eof then begin
      Location.echo_eof ();
      if len > 0 then got_eof := true;
      len
    end else
      len
  end

(* Toplevel initialization. Performed here instead of at the
   beginning of loop() so that user code linked in with ocamlmktop
   can call directives from Topdirs. *)

let _ =
  Sys.interactive := true;
  Compmisc.init_path ();
  Clflags.dlcode := true;
  ()

let find_ocamlinit () =
  let ocamlinit = ".ocamlinit" in
  if Sys.file_exists ocamlinit then Some ocamlinit else
  let getenv var = match Sys.getenv var with
    | exception Not_found -> None | "" -> None | v -> Some v
  in
  let exists_in_dir dir file = match dir with
    | None -> None
    | Some dir ->
        let file = Filename.concat dir file in
        if Sys.file_exists file then Some file else None
  in
  let home_dir () = getenv "HOME" in
  let config_dir () =
    if Sys.win32 then None else
    match getenv "XDG_CONFIG_HOME" with
    | Some _ as v -> v
    | None ->
        match home_dir () with
        | None -> None
        | Some dir -> Some (Filename.concat dir ".config")
  in
  let init_ml = Filename.concat "ocaml" "init.ml" in
  match exists_in_dir (config_dir ()) init_ml with
  | Some _ as v -> v
  | None -> exists_in_dir (home_dir ()) ocamlinit

let load_ocamlinit ppf =
  if !Clflags.noinit then ()
  else match !Clflags.init_file with
  | Some f -> if Sys.file_exists f then ignore (use_silently ppf f)
              else fprintf ppf "Init file not found: \"%s\".@." f
  | None ->
      match find_ocamlinit () with
      | None -> ()
      | Some file -> ignore (use_silently ppf file)
;;

let set_paths () =
  (* Add whatever -I options have been specified on the command line,
     but keep the directories that user code linked in with ocamlmktop
     may have added to load_path. *)
  let expand = Misc.expand_directory Config.standard_library in
  let Load_path.{ visible; hidden } = Load_path.get_paths () in
  let visible = List.concat [
      [ "" ];
      List.map expand (List.rev !Compenv.first_include_dirs);
      List.map expand (List.rev !Clflags.include_dirs);
      List.map expand (List.rev !Compenv.last_include_dirs);
      visible;
      [expand "+camlp4"];
    ]
  in
  let hidden = List.concat [
      List.map expand (List.rev !Clflags.hidden_include_dirs);
      hidden
    ]
  in
  Load_path.init ~auto_include:Compmisc.auto_include ~visible ~hidden

let initialize_toplevel_env () =
  toplevel_env := Compmisc.initial_env();
  toplevel_sig := []

(* The interactive loop *)

exception PPerror

let loop ppf =
  Location.formatter_for_warnings := ppf;
  if not !Clflags.noversion then
    fprintf ppf "        OCaml version %s - native toplevel@.@." Config.version;
  initialize_toplevel_env ();
  let lb = Lexing.from_function refill_lexbuf in
  Location.init lb "//toplevel//";
  Location.input_name := "//toplevel//";
  Location.input_lexbuf := Some lb;
  Sys.catch_break true;
  run_hooks After_setup;
  load_ocamlinit ppf;
  while true do
    let snap = Btype.snapshot () in
    try
      Lexing.flush_input lb;
      Location.reset();
      first_line := true;
      let phr = try !parse_toplevel_phrase lb with Exit -> raise PPerror in
      let phr = preprocess_phrase ppf phr  in
      Env.reset_cache_toplevel ();
      if !Clflags.dump_parsetree then Printast.top_phrase ppf phr;
      if !Clflags.dump_source then Pprintast.top_phrase ppf phr;
      ignore(execute_phrase true ppf phr)
    with
    | End_of_file -> raise (Compenv.Exit_with_status 0)
    | Sys.Break -> fprintf ppf "Interrupted.@."; Btype.backtrack snap
    | PPerror -> ()
    | x -> Location.report_exception ppf x; Btype.backtrack snap
  done

external caml_sys_modify_argv : string array -> unit =
  "caml_sys_modify_argv"

let override_sys_argv new_argv =
  caml_sys_modify_argv new_argv;
  Arg.current := 0

(* Execute a script.  If [name] is "", read the script from stdin. *)

let run_script ppf name args =
  override_sys_argv args;
  Compmisc.init_path ~dir:(Filename.dirname name) ();
                   (* Note: would use [Filename.abspath] here, if we had it. *)
  initialize_toplevel_env ();
  Sys.interactive := false;
  run_hooks After_setup;
  let explicit_name =
    (* Prevent use_silently from searching in the path. *)
    if Filename.is_implicit name
    then Filename.concat Filename.current_dir_name name
    else name
  in
  use_silently ppf explicit_name
