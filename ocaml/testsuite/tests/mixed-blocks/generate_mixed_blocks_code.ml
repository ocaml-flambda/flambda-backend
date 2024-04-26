(* The below code recursively enumerates all mixed record types,
   takes a finite prefix of that, and prints a program that
   uses the mixed record types in interesting ways that exercise
   corners of the runtime. (Especially polymorphic operations,
   copying, and garbage collection.)

   It is used in [test.ml].
*)

let printf = Printf.printf
let sprintf = Printf.sprintf

module List = ListLabels
module String = StringLabels

module Nonempty_list = struct
  type 'a t = ( :: ) of 'a * 'a list

  let of_list = function
    | [] -> None
    | x :: xs -> Some (x :: xs)
  ;;

  let to_list (x :: xs) : _ list = x :: xs

  let map t ~f = of_list (List.map (to_list t) ~f) |> Option.get
end

let rec enumeration_of_list enumeration_of_x =
  Seq.cons [] (fun () ->
    let f =
      Seq.product (enumeration_of_list enumeration_of_x) enumeration_of_x
      |> Seq.map (fun (xs, x) -> x :: xs)
    in
    f ())
;;

let enumeration_of_nonempty_list enumeration_of_x =
  enumeration_of_list enumeration_of_x |> Seq.filter_map Nonempty_list.of_list
;;

type flat_element =
  | Imm
  | Float_u
  | Float

let flat_element_is_float = function
  | Float_u | Float -> true
  | Imm -> false

let flat_element_is = ((=) : flat_element -> flat_element -> bool)
let flat_element_is_not = ((<>) : flat_element -> flat_element -> bool)

let all_of_flat_element = [ Imm; Float_u; Float ]

type value_element =
  | Str
  | Float
  | Imm

let value_element_is = ((=) : value_element -> value_element -> bool)

let all_of_value_element = [ Str; Float; Imm ]

type mutability =
  | Mutable
  | Immutable

let all_of_mutability = [ Mutable; Immutable ]

let is_mutable = function
  | Mutable -> true
  | Immutable -> false
;;

let enumeration_of_flat_element = List.to_seq all_of_flat_element
let enumeration_of_value_element = List.to_seq all_of_value_element
let enumeration_of_mutability = List.to_seq all_of_mutability

type prefix = (value_element * mutability) list
type suffix = (flat_element * mutability) Nonempty_list.t

let enumeration_of_prefix enumeration_of_mutability =
  enumeration_of_list
    (Seq.product enumeration_of_value_element enumeration_of_mutability)
  |> Seq.filter (fun prefix ->
    match List.rev prefix with
    | [] -> true
    | (Imm, _) :: _ -> false
    | ((Str | Float), _) :: _ -> true)
;;

let enumeration_of_suffix_except_all_floats_mixed
    enumeration_of_mutability
  : _ Seq.t
  =
  let flat_element_except_float =
    enumeration_of_flat_element
    |> Seq.filter (flat_element_is_not Float)
  in
  enumeration_of_nonempty_list
    (Seq.product flat_element_except_float enumeration_of_mutability)
  |> Seq.filter (fun suffix ->
    List.exists (Nonempty_list.to_list suffix) ~f:(fun (elem, _) ->
      flat_element_is Float_u elem))
;;

let enumeration_of_all_floats_mixed_suffix =
  let float_flat_element =
    enumeration_of_flat_element
    |> Seq.filter flat_element_is_float
  in
  enumeration_of_nonempty_list
    (Seq.product float_flat_element enumeration_of_mutability)
  |> Seq.filter (fun suffix ->
      let suffix = Nonempty_list.to_list suffix in
      List.exists suffix ~f:(fun (elem, _) -> flat_element_is Float_u elem)
      && List.exists suffix ~f:(fun (elem, _) -> flat_element_is Float elem))

type block =
  { prefix : prefix
  ; suffix : suffix
  }

let enumeration_of_mixed_blocks_except_all_floats_mixed
    enumeration_of_mutability
  =
  Seq.product
   (enumeration_of_prefix enumeration_of_mutability)
   (enumeration_of_suffix_except_all_floats_mixed enumeration_of_mutability)
  |> Seq.filter (fun (prefix, suffix) ->
      let all_float_u_suffix =
        List.for_all (Nonempty_list.to_list suffix)
          ~f:(fun (elem, _) -> flat_element_is Float_u elem)
      in
      let all_float_prefix =
        List.for_all prefix
          ~f:(fun (elem, _) -> value_element_is Float elem)
      in
      not all_float_u_suffix || not all_float_prefix)

type constructor =
  { cstr_prefix : value_element list;
    cstr_suffix : flat_element Nonempty_list.t;
  }

type variant = constructor Nonempty_list.t

let enumeration_of_mixed_variants =
  enumeration_of_mixed_blocks_except_all_floats_mixed
      (List.to_seq [ `Mutability_not_relevant ])
  |> Seq.map (fun (prefix, suffix) ->
      let ignore_mut (x, `Mutability_not_relevant) = x in
      { cstr_prefix = List.map prefix ~f:ignore_mut;
        cstr_suffix = Nonempty_list.map suffix ~f:ignore_mut;
      })
  |> enumeration_of_nonempty_list

let enumeration_of_mixed_blocks_except_all_floats_mixed =
  enumeration_of_mixed_blocks_except_all_floats_mixed
    enumeration_of_mutability
  |> Seq.map (fun (prefix, suffix) -> { prefix; suffix })


let enumeration_of_all_floats_mixed_blocks =
  enumeration_of_all_floats_mixed_suffix
  |> Seq.map (fun suffix -> { prefix = []; suffix })
;;

type field_or_arg_type =
  | Imm
  | Float
  | Float_u
  | Str

module Mixed_record = struct
  type field =
    { type_ : field_or_arg_type
    ; name : string
    ; mutable_ : bool
    }

  type t =
    { index : int
    ; fields : field list
    }

  let is_all_floats t =
    List.for_all t.fields ~f:(fun field ->
        match field.type_ with
        | Imm | Str -> false
        | Float | Float_u -> true)

  let of_block index { prefix; suffix } =
    let num_fields, prefix_fields =
      List.fold_left_map
        prefix
        ~init:0
        ~f:(fun i ((elem : value_element), mutability) ->
          let mutable_ = is_mutable mutability in
          let field =
            match elem with
            | Imm -> { type_ = Imm; name = sprintf "imm%d" i; mutable_ }
            | Float -> { type_ = Float; name = sprintf "float%d" i; mutable_ }
            | Str -> { type_ = Str; name = sprintf "str%d" i; mutable_ }
          in
          i+1, field)
    in
    let _, suffix_fields =
      List.fold_left_map
        (Nonempty_list.to_list suffix)
        ~init:num_fields
        ~f:(fun i ((elem : flat_element), mutability) ->
          let mutable_ = is_mutable mutability in
          let field =
            match elem with
            | Imm -> { type_ = Imm; name = sprintf "imm%d" i; mutable_ }
            | Float ->
                { type_ = Float; name = sprintf "float%d" i; mutable_ }
            | Float_u ->
                { type_ = Float_u; name = sprintf "float_u%d" i; mutable_ }
          in
          i+1, field)
    in
    let fields = prefix_fields @ suffix_fields in
    { fields; index }
  ;;

  let value ?(base = "t") { index; _ } = sprintf "%s%d" base index
  let type_ { index; _ } = sprintf "t%d" index

  let type_decl t =
    sprintf
      "type %s = { %s }"
      (type_ t)
      (String.concat
         ~sep:"; "
         (List.map t.fields ~f:(fun { type_; name; mutable_ } ->
            sprintf
              "%s%s : %s"
              (if mutable_ then "mutable " else "")
              name
              (match type_ with
               | Imm -> "int"
               | Float -> "float"
               | Float_u -> "float#"
               | Str -> "string"))))
  ;;

  let record_value t =
    String.concat
      ~sep:"; "
      (List.map t.fields ~f:(fun { type_; name; mutable_ = _ } ->
         sprintf
           "%s = %s"
           name
           (match type_ with
            | Imm -> "create_int ()"
            | Float -> "create_float ()"
            | Float_u -> "create_float_u ()"
            | Str -> "create_string ()")))
    |> sprintf "{ %s }"
  ;;

  let check_field_integrity t =
    List.map t.fields ~f:(fun field ->
      let checker, transformation =
        match field.type_ with
        | Str -> "check_string", None
        | Imm -> "check_int", None
        | Float -> "check_float", None
        | Float_u -> "check_float", Some "Stdlib__Float_u.to_float"
      in
      let transform value field =
        let access = sprintf "%s.%s" value field in
        match transformation with
        | None -> access
        | Some f -> sprintf "(%s %s)" f access
      in
      sprintf
        "%s %s %s ~message:\"%s.%s\";"
        checker
        (transform (value t) field.name)
        (transform (value t ~base:"t_orig") field.name)
        (value t)
        field.name)
  ;;
end

module Mixed_variant = struct
  type constructor =
    { name : string;
      args : field_or_arg_type list;
    }

  type t =
    { index : int
    ; constructors : constructor list
    }

  let of_constructor name { cstr_prefix; cstr_suffix } =
    let num_args, prefix_args =
      List.fold_left_map
        cstr_prefix
        ~init:0
        ~f:(fun i (elem : value_element) ->
          let arg =
            match elem with
            | Imm -> Imm
            | Float -> Float
            | Str -> Str
          in
          i+1, arg)
    in
    let _, suffix_args =
      List.fold_left_map
        (Nonempty_list.to_list cstr_suffix)
        ~init:num_args
        ~f:(fun i (elem : flat_element) ->
          let arg =
            match elem with
            | Imm -> Imm
            | Float -> Float
            | Float_u -> Float_u
          in
          i+1, arg)
    in
    let args = prefix_args @ suffix_args in
    { name; args }
  ;;

  let of_variant index cstrs =
    let constructors =
      Nonempty_list.to_list cstrs
      |> List.mapi ~f:(fun i cstr ->
        let name = sprintf "%c" (Char.chr (Char.code 'A' + i)) in
        of_constructor name cstr)
    in
    { index; constructors; }

  let constructor_decl cstr =
    sprintf
      "  | %s of %s"
      cstr.name
      (String.concat
         ~sep:" * "
         (List.map cstr.args ~f:(fun type_ ->
            match type_ with
              | Imm -> "int"
              | Float -> "float"
              | Float_u -> "float#"
              | Str -> "string")))
  ;;

  let type_ { index; _ } = sprintf "t%d" index
  let value ?(base = "t") { name; _ } ~index = sprintf "%s%d_%s" base index name

  let constructor_value { name; args } =
    sprintf "(%s %s)"
      name
      (let args_str =
        String.concat
        ~sep:", "
        (List.map args ~f:(fun type_ ->
            match type_ with
            | Imm -> "create_int ()"
            | Float -> "create_float ()"
            | Float_u -> "create_float_u ()"
            | Str -> "create_string ()"))
        in
        match args with
        | [] | [_] -> args_str
        | _ -> sprintf "(%s)" args_str)

  let type_decl t =
    sprintf
      "type %s =\n%s"
      (type_ t)
      (String.concat ~sep:"\n" (List.map t.constructors ~f:constructor_decl))
  ;;

  let check_field_integrity cstr ~index ~catchall =
    let value = value ~index in
    let arg_var i ~base = sprintf "%s%i" base i in
    let arg_vars cstr ~base =
      String.concat ~sep:","
        (List.mapi cstr.args ~f:(fun i _ ->
             arg_var i ~base))
    in
    sprintf {|let () = match %s, %s with
      | %s (%s), %s (%s) -> %s
      %s
    in|}
    (value cstr)
    (value cstr ~base:"t_orig")
    cstr.name (arg_vars cstr ~base:"a")
    cstr.name (arg_vars cstr ~base:"b")
    (String.concat ~sep:"\n"
       (List.mapi cstr.args ~f:(fun i (arg : field_or_arg_type) ->
          let checker, transformation =
            match arg with
            | Str -> "check_string", None
            | Imm -> "check_int", None
            | Float -> "check_float", None
            | Float_u -> "check_float", Some "Stdlib__Float_u.to_float"
          in
          let transform base =
            let access = arg_var i ~base in
            match transformation with
            | None -> access
            | Some f -> sprintf "(%s %s)" f access
          in
          sprintf
            "%s %s %s ~message:\"%s.%i\";"
            checker
            (transform "a")
            (transform "b")
            (value cstr)
            i)))
    (if catchall then "| _ -> assert false" else "")
  ;;
end

module Value = struct
  type t =
    | Record of Mixed_record.t
    | Constructor of Mixed_variant.t * Mixed_variant.constructor

  let value ?base = function
    | Record x -> Mixed_record.value ?base x
    | Constructor (v, c) -> Mixed_variant.value ?base c ~index:v.index

  let type_ = function
    | Record x -> Mixed_record.type_ x
    | Constructor (x, _) -> Mixed_variant.type_ x

  let construction = function
    | Record x -> Mixed_record.record_value x
    | Constructor (_, x) -> Mixed_variant.constructor_value x

  let is_all_floats = function
    | Record x -> Mixed_record.is_all_floats x
    | Constructor _ -> false

  let index = function
    | Record x -> x.index
    | Constructor (x, _) -> x.index

  let check_field_integrity = function
    | Record x -> Mixed_record.check_field_integrity x
    | Constructor (v, c) ->
        [ Mixed_variant.check_field_integrity c ~index:v.index
            ~catchall:(List.length v.constructors > 1)
        ]
end

module Type = struct
  type t =
    | Record of Mixed_record.t
    | Variant of Mixed_variant.t

  let type_decl = function
    | Record x -> Mixed_record.type_decl x
    | Variant x -> Mixed_variant.type_decl x

  let index = function
    | Record x -> x.index
    | Variant x -> x.index

  let values : t -> Value.t list = function
    | Record x -> [ Record x ]
    | Variant x ->
        List.map x.constructors ~f:(fun cstr -> Value.Constructor (x, cstr))

  let record_of_block i block =
    Record (Mixed_record.of_block i block)

  let variant_of_block i block =
    Variant (Mixed_variant.of_variant i block)
end

let main n ~bytecode =
  (* Don't overrepresent all-float mixed blocks. *)
  let n_all_floats_mixed = n / 4 in
  let records =
    Seq.append
      (Seq.take n_all_floats_mixed
        enumeration_of_all_floats_mixed_blocks)
      (Seq.take (n - n_all_floats_mixed)
        enumeration_of_mixed_blocks_except_all_floats_mixed)
    |> List.of_seq
    |> List.mapi ~f:Type.record_of_block
  in
  let variants =
    Seq.take n enumeration_of_mixed_variants
    |> List.of_seq
    |> List.mapi ~f:(fun i x -> Type.variant_of_block (i+n) x)
  in
  let types = records @ variants in
  let values = List.concat_map types ~f:Type.values in
  let line ?(indent = 0) fmt =
    Printf.ksprintf
      (fun s ->
         let indent = Seq.init indent (fun _ -> ' ') |> String.of_seq in
         print_endline (indent ^ s))
      fmt
  in
  let print_in_test ?indent s =
    line ?indent {|let () = print_endline "%s";;|} (String.escaped s)
  in
  let seq_print_in_test ?indent s =
    line ?indent {|print_endline "%s";|} (String.escaped s)
  in
  let do_gc ?indent () =
    print_in_test ?indent " - Doing GC";
    line ?indent "let () = Gc.full_major ();;"
  in
  let per_type f = List.iter types ~f in
  let per_value f = List.iter values ~f in
  (* Iterate over the list of values zipped with itself (staggered by 1) *)
  let per_value_staircase f =
    List.iter2 values (List.tl values @ [ List.hd values]) ~f
  in
  line {|(* TEST
 flags = "-extension layouts_alpha";|};
  if bytecode then (
    line {| bytecode;|};
  ) else (
    line {| flambda2;|};
    line {| native;|};
  );
  line {|*)|};
  line "(** This is code generated by [generate_mixed_blocks_code.ml]. *)";
  line "";
  line "(* Helper functions for manipulating the fields of a mixed record *)";
  line {|let create_string () = String.make (Random.int 100) 'a'|};
  line {|let create_int () = Random.int 0x3FFF_FFFF|};
  line {|let create_float () = Random.float Float.max_float|};
  line {|let create_float_u () = Stdlib__Float_u.of_float (create_float ())|};
  line
    {|let check_gen ~equal ~to_string ~message y1 y2 =
  if equal y1 y2 then () else
    failwith
      (Printf.sprintf "%%s: %%s <> %%s" message (to_string y1) (to_string y2))
|};
  line
   {|let check_string = check_gen ~equal:String.equal ~to_string:(fun x -> x)|};
  line {|let check_int = check_gen ~equal:Int.equal ~to_string:Int.to_string|};
  line
   {|let check_float =
  check_gen ~equal:Float.equal ~to_string:Float.to_string|};
  line "";
  line "(* Helper functions for testing polymorphic copying. *)";
  line
    {|
let copy_via_weak x =
  let weak = Weak.create 1 in
  Weak.set weak 0 (Some x);
  Weak.get_copy weak 0 |> Option.get|};
  line
    {|
let copy_via_tag x =
  let obj = Obj.repr x in
  Obj.with_tag (Obj.tag obj) obj |> Obj.obj;;|};
  line "";
  line {|(* Helper functions for testing polymorphic operations. *)|};
  line {|let oc = Out_channel.open_bin "/dev/null"|};
  line {|exception Unexpected_success|};
  line {|type forget = T : _ -> forget|};
  line
    {|
let expect_failure f =
  try f (); raise Unexpected_success with
  | Unexpected_success -> assert false
  | _ -> ()

let try_hash x =
  expect_failure (fun () -> ignore (Hashtbl.hash x : int))

let try_compare x y =
  expect_failure (fun () -> ignore (compare (T x) (T y) : int));
  expect_failure (fun () -> ignore ((T x) = (T y) : bool))

let try_marshal t =
  expect_failure (fun () -> output_value oc t)|};
  line
    {|
let check_reachable_words expected actual message =
  if expected <> actual
  then failwith (Printf.sprintf "%%s: %%d <> %%d" message expected actual)
;;|};
  line "";
  line "(* Type declarations *)";
  per_type (fun t -> line "%s" (Type.type_decl t));
  print_endline "";
  print_endline "(* Let declarations *)";
  print_in_test "Creating values";
  per_value (fun t ->
    line
      "let %s : %s = %s;;"
      (Value.value t)
      (Value.type_ t)
      (Value.construction t));
  do_gc ();
  line "";
  line "(* Copies *)";
  print_in_test "Copying values using [with] record update";
  per_value (fun t ->
    match t with
    | Constructor _ ->
        line
          "let %s = %s;;"
          (Value.value t ~base:"t_orig")
          (Value.value t)
    | Record record ->
      let field = List.hd record.fields in
      line
        "let %s = { %s with %s = %s.%s };;"
        (Value.value t ~base:"t_orig")
        (Value.value t)
        field.name
        (Value.value t)
        field.name);
  print_endline "";
  print_endline "(* Checks *)";
  let () =
    let indent = 2 in
    let line ?(indent = indent) = line ~indent in
    let seq_print_in_test ?(indent = indent) = seq_print_in_test ~indent in
    line
      "let run_checks %s ="
      (List.map values ~f:(fun t ->
         sprintf "(%s : %s)" (Value.value t) (Value.type_ t))
       |> String.concat ~sep:" ");
    seq_print_in_test "    - Marshaling";
    per_value (fun t -> line "try_marshal %s;" (Value.value t));
    seq_print_in_test "    - Hashing";
    per_value (fun t -> line "try_hash %s;" (Value.value t));
    if n > 1
    then (
      seq_print_in_test "    - Comparing";
      per_value_staircase (fun t1 t2 ->
          line "try_compare %s %s;"
            (Value.value t1)
            (Value.value t2)));
    seq_print_in_test "    - Checking field values";
    per_value (fun t ->
        List.iter (Value.check_field_integrity t) ~f:(line "%s"));
    seq_print_in_test "    - Checking [Obj.reachable_words]";
    per_value (fun t ->
      match t with
      | Constructor _ -> ()
      | Record t ->
        let is_all_floats = Mixed_record.is_all_floats t in
        line
        {|check_reachable_words (Obj.reachable_words (Obj.repr %s)) (%d%s) "Reachable words %d";|}
          (Mixed_record.value t)
          (List.length t.fields + 1)
          (List.map t.fields ~f:(fun (field : Mixed_record.field) ->
            match field.type_ with
            | Imm -> ""
            | Float_u ->
                (* In bytecode, these fields aren't boxed and thus contribute
                    two words to the reachable words (the header and the
                    single-field payload).
                *)
                if not bytecode then "" else " + 2"
            | Float ->
                (* The bytecode condition is the same as commented for [Float_u].
                    Additionally, if the record is not all floats, then this field
                    is stored boxed.
                *)
                if is_all_floats && not bytecode then "" else " + 2"
            | Str ->
                sprintf " + Obj.reachable_words (Obj.repr t%d.%s)"
                  t.index field.name)
          |> String.concat ~sep:"")
          t.index);
    line "();;"
  in
  let run_checks ?indent () =
    print_in_test " - Running checks";
    line
      ?indent
      "let () = run_checks %s;;"
      (List.map values ~f:Value.value |> String.concat ~sep:" ")
  in
  run_checks ();
  do_gc ();
  run_checks ();
  print_in_test "Copying values via [Stdlib.Weak]";
  per_value (fun t ->
    line
      "let %s : %s = copy_via_weak %s"
      (Value.value t)
      (Value.type_ t)
      (Value.value t));
  run_checks ();
  do_gc ();
  run_checks ();
  print_in_test "Copying values via [Obj.with_tag]";
  per_value (fun t ->
    line
      "let %s : %s = copy_via_tag %s"
      (Value.value t)
      (Value.type_ t)
      (Value.value t));
  run_checks ();
  do_gc ();
  run_checks ();
  line "";
  line "(* Testing local allocation *)";
  line {|external opaque_ignore : ('a [@local_opt]) -> unit = "%%opaque"|};
  print_endline "let go () =";
  let () =
    let indent = 2 in
    let line ?(indent = indent) = line ~indent in
    per_value (fun t ->
      line
        "let local_ %s : %s = %s in"
        (Value.value t)
        (Value.type_ t)
        (Value.construction t));
    line "  let module _ = struct";
    do_gc () ~indent:4;
    line "end in";
    per_value (fun t -> line "opaque_ignore %s;" (Value.value t));
    line "();;"
  in
  print_in_test "Testing local allocations";
  print_endline "let () = go ();;"
;;

let () =
  let n, bytecode =
    match Sys.argv with
    | [| _; n; "native" |] -> n, false
    | [| _; n; "byte" |] -> n, true
    | _ -> failwith (Printf.sprintf "Usage: %s N <byte|native>" Sys.argv.(0))
  in
  main (int_of_string n) ~bytecode
