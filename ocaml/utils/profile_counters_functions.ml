type typing_output_for_counters =
  | Typedtree_implementation_output of Typedtree.implementation
  | Typedtree_signature_output of Typedtree.signature

let count_language_extensions typing_input =
  let counters = ref (Profile.Counters.create ()) in
  let to_string : type a. a Language_extension_kernel.t -> string =
   fun lang_ext ->
    match lang_ext with
    | Comprehensions | Include_functor | Immutable_arrays | Module_strengthening
    | Labeled_tuples ->
      Language_extension_kernel.to_string lang_ext
    | Mode | Unique | Overwriting | Polymorphic_parameters | Layouts | SIMD
    | Small_numbers ->
      let error_msg =
        Format.sprintf "No counters supported for language extension : %s."
          (Language_extension_kernel.to_string lang_ext)
      in
      failwith error_msg
  in
  let incr lang_ext =
    counters := Profile.Counters.incr (to_string lang_ext) !counters
  in
  (* CR-someday mitom: Add support for counting stdlib [Iarray] usages *)
  let supported_lang_exts =
    Language_extension_kernel.
      [ Comprehensions;
        Include_functor;
        Immutable_arrays;
        Module_strengthening;
        Labeled_tuples;
        Immutable_arrays ]
  in
  List.iter
    (fun lang_ext ->
      counters := Profile.Counters.set (to_string lang_ext) 0 !counters)
    supported_lang_exts;
  let check_for_labeled_tuples label_opt_pair_list =
    if List.exists
         (fun (label_opt, _) -> Option.is_some label_opt)
         label_opt_pair_list
    then incr Labeled_tuples
  in
  let check_array_mutability mutability =
    if not (Types.is_mutable mutability) then incr Immutable_arrays
  in
  let iterator =
    Tast_iterator.
      { default_iterator with
        structure_item =
          (fun sub ({ str_desc; _ } as si) ->
            (match str_desc with
            | Tstr_include include_declaration -> (
              match include_declaration.incl_kind with
              | Tincl_functor _ | Tincl_gen_functor _ -> incr Include_functor
              | Tincl_structure -> ())
            | _ -> ());
            default_iterator.structure_item sub si);
        signature_item =
          (fun sub ({ sig_desc; _ } as si) ->
            (match sig_desc with
            | Tsig_include (include_declaration, _) -> (
              match include_declaration.incl_kind with
              | Tincl_functor _ | Tincl_gen_functor _ -> incr Include_functor
              | Tincl_structure -> ())
            | _ -> ());
            default_iterator.signature_item sub si);
        expr =
          (fun sub ({ exp_desc; _ } as e) ->
            (match exp_desc with
            | Texp_list_comprehension _ -> incr Comprehensions
            | Texp_array_comprehension (mutability, _, _) ->
              incr Comprehensions;
              check_array_mutability mutability
            | Texp_array (mutability, _, _, _) ->
              check_array_mutability mutability
            | Texp_tuple (label_opt_pair_list, _) ->
              check_for_labeled_tuples label_opt_pair_list
            | _ -> ());
            default_iterator.expr sub e);
        module_type =
          (fun sub ({ mty_desc; _ } as mty) ->
            (match mty_desc with
            | Tmty_strengthen _ -> incr Module_strengthening
            | _ -> ());
            default_iterator.module_type sub mty);
        typ =
          (fun sub ({ ctyp_desc; _ } as ctyp) ->
            (match ctyp_desc with
            | Ttyp_tuple label_opt_pair_list ->
              check_for_labeled_tuples label_opt_pair_list
            (* CR-someday mitom: type occurence of [iarray] double counted in
               [let a_iarray : int iarray = [: 1; 2; 3; 4; 5 :]] *)
            | Ttyp_constr (Pident ident, _, _) ->
              if Ident.is_predef ident
                 && String.equal (Ident.name ident) "iarray"
              then incr Immutable_arrays
            | _ -> ());
            default_iterator.typ sub ctyp);
        pat =
          (fun (type k) sub
               ({ pat_desc; _ } as gen_pat : k Typedtree.general_pattern) ->
            (match pat_desc with
            | Tpat_tuple label_opt_pair_list ->
              check_for_labeled_tuples label_opt_pair_list
            | Tpat_array (mutability, _, _) -> check_array_mutability mutability
            | _ -> ());
            default_iterator.pat sub gen_pat)
      }
  in
  (match typing_input with
  | Typedtree_implementation_output tree ->
    iterator.structure iterator tree.structure
  | Typedtree_signature_output signature ->
    iterator.signature iterator signature);
  !counters
