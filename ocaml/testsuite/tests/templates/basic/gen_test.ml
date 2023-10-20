open! StdLabels

let test_tree_token = "<TEST TREE HERE>"

type test_tree =
  | Act of string * (string * string) list
  | Seq of test_tree list
  | Par of test_tree list
  | Nop
  | Branch of test_tree

let stars n =
  String.make n '*'

let backslash_newlines s =
  let lines = String.split_on_char ~sep:'\n' s in
  String.concat ~sep:"\\\n" lines

let out_test_tree ppf tree =
  let rec out n tree after =
    match tree with
    | Act (action, vars) ->
      Format.fprintf ppf "%s %s\n" (stars n) action;
      List.iter vars ~f:(fun (var, value) ->
        Format.fprintf ppf "%s = \"%s\"\n" var (backslash_newlines value));
      out_next (n+1) after
    | Seq trees -> out_next n (trees @ after)
    | Par trees ->
      begin
        match after with
        | [] -> List.iter trees ~f:(fun tree -> out n tree [])
        | _ -> failwith "Par in middle of Seq"
      end
    | Nop -> out_next n after
    | Branch tree ->
      out n tree [];
      out_next n after
  and out_next n after =
    match after with
    | [] -> ()
    | tree :: trees ->
      out n tree trees
  in
  out 1 tree []

let generate ~in_ ~out test_tree =
  In_channel.with_open_text in_ @@ fun ic ->
    Out_channel.with_open_text out @@ fun oc ->
      let ppf = Format.formatter_of_out_channel oc in
      let rec loop () =
        match In_channel.input_line ic with
        | Some line ->
          begin match String.equal line test_tree_token with
          | true -> out_test_tree ppf test_tree
          | false -> Format.fprintf ppf "%s\n" line
          end;
          loop ()
        | None -> ()
      in
      loop ();
      Format.pp_print_flush ppf ()

type mode = Byte | Native

let (!%) = Format.sprintf

let sep_unless_empty s1 s2 =
  match s1, s2 with
  | "", _ -> s2
  | _, "" -> s1
  | _, _ -> !%"%s %s" s1 s2

let tree_for_mode mode =
  let ocamlc =
    match mode with
    | Byte -> "ocamlc"
    | Native -> "ocamlopt"
  in
  let cmo =
    match mode with
    | Byte -> "cmo"
    | Native -> "cmx"
  in
  let bc =
    match mode with
    | Byte -> "bc"
    | Native -> "exe"
  in
  let compiler = !%"%s.byte" ocamlc in
  let compiler_under = !%"%s_byte" ocamlc in
  let add_flags ~flags vars =
    match flags with
    | None -> vars
    | Some flags -> ("flags", flags) :: vars
  in
  let add_extra ?(extra = []) vars = extra @ vars in
  let compile ?flags ?extra filenames =
    Act (compiler, add_flags ~flags (add_extra ?extra [
      "module", filenames;
    ]))
  in
  let compile_bad ~ext ?flags module_ =
    Branch (Seq [
      Act (compiler, add_flags ~flags [
        "module", !%"%s.%s" module_ ext;
        "compiler_output", !%"%s.output" module_;
        !%"%s_exit_status" compiler_under, "2";
      ]);
      Act (!%"check-%s-output" compiler, [
        "compiler_reference", !%"%s.reference" module_
      ]);
    ])
  in
  let compile_bad_ml = compile_bad ~ext:"ml" in
  let compile_bad_mli = compile_bad ~ext:"mli" in
  let link ?flags main modules =
    let all_modules =
      List.concat [
        [ "" ];
        List.map modules ~f:(fun m -> !%"   %s.%s " m cmo);
        [ "" ];
      ]
      |> String.concat ~sep:"\n"
    in
    Act (compiler, add_flags ~flags [
      "program", !%"${test_build_directory}/%s.%s" main bc;
      "module", "";
      "all_modules", all_modules
    ])
  in
  let link_and_run ?flags main modules =
    Seq [
      link ?flags main modules;
      Act ("run", [ "output", !%"%s.output" main ]);
      Act ("check-program-output", [ "reference", !%"%s.reference" main ]);
    ]
  in
  Seq [
    Act (!%"setup-%s-build-env" compiler, []);
    compile "monoid.mli" ~flags:"-as-parameter";
    compile_bad_ml "bad_ref_direct" ~flags:"";
    compile_bad_ml "bad_arg_impl" ~flags:"-as-argument-for Monoid";
    compile_bad_mli "bad_arg_intf" ~flags:"-as-argument-for Monoid";
    Branch (Seq [
      Act ("copy", [
        "src", "string_monoid.ml";
        "dst", "string_monoid_no_mli.ml"
      ]);
      compile "string_monoid_no_mli.ml string_monoid.mli string_monoid.ml"
        ~flags:"-as-argument-for Monoid";
      Branch (Seq [
        compile "test_direct_access.ml" ~flags:"";
        link_and_run "test_direct_access" ~flags:""
          ["string_monoid"; "string_monoid_no_mli"; "test_direct_access"];
      ]);
    ]);
    compile "semigroup.mli";
    compile "category.mli";
    compile "monoid_of_semigroup.mli"
      ~flags:"-parameter Semigroup -as-argument-for Monoid";
    (* Invoke the compiler separately on .mli and .ml just this once to make sure
       things work this way as well *)
    compile "monoid_of_semigroup.ml";
    compile "list_element.mli" ~flags:"-as-parameter";
    compile "list_monoid.mli list_monoid.ml"
      ~flags:"-parameter List_element -as-argument-for Monoid";
    compile "monoid_utils.mli monoid_utils.ml" ~flags:"-parameter Monoid";
    compile_bad_ml "bad_ref_indirect" ~flags:"";
    compile_bad_ml "bad_instance_arg_name_not_found"
      ~flags:"-parameter List_element";
    compile_bad_ml "bad_instance_arg_value_not_arg"
      ~flags:"-parameter List_element";
    compile_bad_ml "bad_instance_arg_value_not_found"
      ~flags:"-parameter List_element";
    compile_bad_ml "bad_ref_direct_imported" ~flags:"-parameter Semigroup";
    compile "chain.mli chain.ml" ~flags:"-parameter Category";
    compile "category_utils.mli category_utils.ml" ~flags:"-parameter Category";
    compile "category_of_monoid.mli category_of_monoid.ml"
      ~flags:"-parameter Monoid -as-argument-for Category";
    compile_bad_ml "bad_instance_arg_value_wrong_type"
      ~flags:"-parameter List_element";
    compile "import.ml"
      ~flags:"-parameter Semigroup -parameter List_element -w -misplaced-attribute";
    compile "main.mli"
      ~flags:"-parameter Semigroup -parameter List_element -w -misplaced-attribute";
    Branch (Seq [
      Act (compiler, [
        "flags", "-parameter Semigroup -parameter List_element -w -misplaced-attribute -i";
        "module", "main.ml";
      ]);
      Act (!%"check-%s-output" compiler, [
        "compiler_reference", "main.reference"
      ]);
    ]);
    compile "main.ml";
    Branch (
      match mode with
      | Byte -> Seq [
          Act ("ocamlobjinfo", [
            "program", !%"main.%s main.cmi" cmo;
          ]);
          Act ("check-program-output", [])
        ]
      | Native ->
        (* flambda output is too noisy *)
        Nop
    );
  ]

let test_tree = Par [ tree_for_mode Byte; tree_for_mode Native ]

let () = generate ~in_:"test.in.ml" ~out:"test.ml" test_tree
