(* TEST
 include ocamlcommon;
 flags = "-I ${ocamlsrcdir}/parsing";
*)

(* See [ocaml/testsuite/tests/parsetree/source_jane_street.ml] for tests for
   new language extensions. The present test is along a different axis: the
   different syntax elements that we support in Pprintast.
*)

(******************************************************************************)
(* Setup *)

let () = Language_extension.set_universe_and_enable_all
  Language_extension.Universe.maximal;;

module Example = struct
  open Parsetree
  open Parse
  open struct
    let loc = Location.none
    let located =  Location.mknoloc
    let parse p str = p (Lexing.from_string str)
  end

  let longident        = parse longident "No.Longidents.Require.extensions"
  let expression       = parse expression "[x for x = 1 to 10]"
  let pattern          = parse pattern "[:_:]"
  let core_type        = parse core_type "local_ ('a : value) -> unit"
  let signature        = parse interface "include functor F"
  let structure        = parse implementation "include functor F"
  let module_expr      = parse module_expr "struct include functor F end"
  let toplevel_phrase  = parse toplevel_phrase "#2.17;;"
  let class_field      = { pcf_desc = Pcf_initializer expression
                         ; pcf_loc = loc
                         ; pcf_attributes = []
                         }
  let class_type_field = { pctf_desc = Pctf_constraint (core_type, core_type)
                         ; pctf_loc = loc
                         ; pctf_attributes = []
                         }
  let class_expr       = { pcl_desc =
                             Pcl_constr (located longident, [core_type])
                         ; pcl_loc = loc
                         ; pcl_attributes = []
                         }
  let class_type       = { pcty_desc =
                             Pcty_constr (located longident, [core_type])
                         ; pcty_loc = loc
                         ; pcty_attributes = []
                         }
  let module_type      = parse module_type "sig include functor F end"
  let structure_item   = { pstr_desc = Pstr_eval (expression, [])
                         ; pstr_loc = loc
                         }
  let signature_item   = { psig_desc =
                             Psig_module
                               { pmd_name = located (Some "M")
                               ; pmd_type = module_type
                               ; pmd_attributes = []
                               ; pmd_loc = loc
                               }
                         ; psig_loc = loc
                         }
  let value_binding    = { pvb_pat = pattern
                         ; pvb_expr = expression
                         ; pvb_attributes = []
                         ; pvb_loc = loc
                         ; pvb_constraint = None
                         ; pvb_modes = []
                         }
  let payload          = PStr structure
  let class_signature  = { pcsig_self = core_type
                         ; pcsig_fields = [ class_type_field ]
                         }
  let type_declaration = { ptype_name = located "t"
                         ; ptype_params = []
                         ; ptype_cstrs = []
                         ; ptype_kind = Ptype_abstract
                         ; ptype_private = Public
                         ; ptype_manifest = Some core_type
                         ; ptype_attributes = []
                         ; ptype_loc = loc
                         ; ptype_jkind_annotation =
                             Some
                               { pjkind_loc = loc;
                                 pjkind_desc =  Default;
                               }
                         }
  let tyvar            = "no_tyvars_require_extensions"
  let tyvar_of_name    = "no_tyvars_require_extensions"
  let jkind_annotation : jkind_annotation =
    { pjkind_loc = loc;
      pjkind_desc =
        With
          ( { pjkind_loc = loc;
              pjkind_desc = Abbreviation "value";
            }
          , core_type );
    }

  let mode = { Location.txt = (Parsetree.Mode "global"); loc }
end

let print_test_header name =
  Format.printf "##### %s@;%s@." name (String.make 32 '-')
;;

let print_test_separator () =
  Format.printf "@.%s@.@."
    (String.init 75 (fun i -> if i mod 2 = 0 then '*' else ' '))
;;

module type Test = sig
  val name : string
  val setup : unit -> unit
end

module Print_all (Test : Test) () : sig
  (* Ensure that we test every export of [Pprintast] *)
  include module type of Pprintast
end = struct
  open Pprintast
  type nonrec space_formatter = space_formatter

  let print_test_case name printer wrap_value value =
    let pp f x =
      try printer f (wrap_value x)
      with Jane_syntax_parsing.Error.Error _ ->
        Format.fprintf f "JANE SYNTAX ERROR FROM PPRINTAST"
    in
    Format.printf "@.@[<2>%s:@;%a@]@." name pp value
  ;;

  let test name pp value =
    print_test_case name pp Fun.id value;
    pp
  ;;

  let test_string_of name string_of value =
    print_test_case name Format.pp_print_string string_of value;
    string_of
  ;;

  let () =
    print_test_header Test.name;
    Test.setup ()
  ;;

  let longident = test "longident" longident Example.longident
  let expression = test "expression" expression Example.expression
  let pattern = test "pattern" pattern Example.pattern
  let core_type = test "core_type" core_type Example.core_type
  let signature = test "signature" signature Example.signature
  let structure = test "structure" structure Example.structure
  let module_expr = test "module_expr" module_expr Example.module_expr
  let toplevel_phrase = test "toplevel_phrase" toplevel_phrase Example.toplevel_phrase
  let top_phrase = test "top_phrase" top_phrase Example.toplevel_phrase
  let class_field = test "class_field" class_field Example.class_field
  let class_type_field = test "class_type_field" class_type_field Example.class_type_field
  let class_expr = test "class_expr" class_expr Example.class_expr
  let class_type = test "class_type" class_type Example.class_type
  let module_type = test "module_type" module_type Example.module_type
  let structure_item = test "structure_item" structure_item Example.structure_item
  let signature_item = test "signature_item" signature_item Example.signature_item
  let binding = test "binding" binding Example.value_binding
  let payload = test "payload" payload Example.payload
  let class_signature = test "class_signature" class_signature Example.class_signature
  let type_declaration = test "type_declaration" type_declaration Example.type_declaration

  let string_of_expression = test_string_of "string_of_expression" string_of_expression Example.expression
  let string_of_structure = test_string_of "string_of_structure" string_of_structure Example.structure

  let tyvar_of_name =
    test_string_of "tyvar_of_name" tyvar_of_name Example.tyvar_of_name
  let tyvar = test "tyvar" tyvar Example.tyvar
  let jkind_annotation = test "jkind" jkind_annotation Example.jkind_annotation
  let mode = test "mode" mode Example.mode
end


(******************************************************************************)
(* Tests *)

(* [Pprintast] can correctly print when the extension is enabled. *)
module _ =
  Print_all
    (struct
      let name = "All extensions enabled"
      let setup () = Language_extension.set_universe_and_enable_all
        Language_extension.Universe.maximal
    end)
    ()
;;

let () = print_test_separator ();;

(* [Pprintast] can correctly print when the extension is disabled. *)
module _ =
  Print_all
    (struct
      let name = "Extensions disallowed"
      let setup () = Language_extension.set_universe_and_enable_all No_extensions
    end)
    ()
;;

let () = print_test_separator ();;

(* Can't call [Language_extension.For_pprintast.make_printer_exporter]. *)
let () =
  print_test_header
    "Calling [Language_extension.For_pprintast.make_printer_exporter ()]";
  Format.print_newline ();
  begin match Language_extension.For_pprintast.make_printer_exporter () with
  | _ ->
    Format.printf "INCORRECT SUCCESS"
  | exception Misc.Fatal_error ->
    Format.printf "Correctly raised a fatal error"
  end;
  Format.print_newline ()
;;
