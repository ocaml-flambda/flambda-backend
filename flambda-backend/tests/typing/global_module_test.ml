module Test_data = struct
  open Global_module

  open struct
    let args_of_pairs pairs =
      List.map (fun (param, value) -> { Argument.param; value }) pairs

    let g ?(vis = []) ?(hid = []) head =
      create_exn head (args_of_pairs vis) ~hidden_args:hid

    let p s = Parameter_name.of_string s
  end

  let [@ocamlformat "disable"] () =
    (* This [let] is only here so that ocamlformat lets us add line breaks in
       the following. Note that, despite being disabled, ocamlformat insists on
       adding whitespace errors by indenting empty lines. *)
  
    (* Here we imagine the following modules and parameters, with their
       parameters given in square brackets:
  
       {v
         - X (parameter)
         - Y (parameter)
         - A : X
         - B : Y
         - M[X][Y] (regular module)
         - I (parameter)
         - O (parameter)
         - Conv (parameter)
         - Unit : I or O
         - String : I or O
         - Option[I] : I
         - Opaque[I] : Conv
         - Print[I][Conv] (regular module)
       v}
  
       Each untagged identifier is a [Parameter_name.t] and each [*_g] is a
       [Global_module.t]. *)
    ()

  let x = p "X"

  let x_g = g "X"

  let y = p "Y"

  let y_g = g "Y"

  let a_g = g "A"

  let b_g = g "B"

  let m_g = g "M" ~hid:[x; y]

  let i = p "I"

  let o = p "O"

  let conv = p "Conv"

  let unit_g = g "Unit"

  let string_g = g "String"

  let option_g = g "Option" ~hid:[i]

  let applied_option_g arg = g "Option" ~vis:[i, arg]

  let opaque_g = g "Opaque" ~hid:[i]

  let print_g = g "Print" ~hid:[i; conv]

  let print_g_opaque = g "Print" ~hid:[i] ~vis:[conv, opaque_g]
end

module Subst_tests = struct
  open struct
    let subst param s =
      Global_module.subst param (s |> Global_module.Parameter_name.Map.of_list)

    let [@ocamlformat "disable"] case glob s =
      let s = s |> Global_module.Parameter_name.Map.of_list in
      let s', _changed = Global_module.subst glob s in
      Format.printf "@[<hv 2>%a@ %a@ =@ %a@]@."
        Global_module.print glob
        (Global_module.Parameter_name.Map.print Global_module.print) s
        Global_module.print s'
  end

  let run () =
    Format.printf "@.Tests of subst:@.@.";
    let open Test_data in
    case x_g [x, unit_g];
    case y_g [x, unit_g];
    let y_unit, _changed = subst y_g [x, unit_g] in
    case y_unit [x, string_g];
    case m_g [x, a_g];
    case m_g [x, a_g; y, b_g];
    case x_g [i, unit_g];
    case y_g [i, unit_g];
    case print_g [i, unit_g];
    case print_g [i, option_g; conv, opaque_g];
    case print_g_opaque [i, unit_g];
    ()
end

module Check_tests = struct
  open struct
    let print_params ppf params =
      let pp_list_body ppf params =
        Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
          Global_module.Parameter_name.print ppf params
      in
      match params with
      | [] -> Format.fprintf ppf "[]"
      | _ :: _ -> Format.fprintf ppf "@[<hov 2>[@ %a@ ]@]" pp_list_body params

    let [@ocamlformat "disable"] case s params =
      let s = s |> Global_module.Parameter_name.Map.of_list in
      Format.printf "@[<hv 2>@[<hv 2>check@ %a@ %a@]@ = %a@]@."
        (Global_module.Parameter_name.Map.print Global_module.print) s
        print_params params
        Format.pp_print_bool (Global_module.check s params)
  end

  let run () =
    Format.printf "@.Tests of check:@.@.";
    let open Test_data in
    (* Trivial case: fine *)
    case [] [];
    (* Fully unapplied: fine *)
    case [] [x];
    (* Argument given but no parameters: bad *)
    case [x, string_g] [];
    (* Exactly one argument and one matching parameter: fine *)
    case [x, string_g] [x];
    (* Exactly one argument and one matching parameter: fine *)
    case [i, unit_g] [i];
    (* Exactly one argument and one non-matching parameter: bad *)
    case [i, unit_g] [o];
    (* Partial application: fine *)
    case [i, unit_g] [conv; i];
    (* Application to parameterized argument: fine *)
    case [i, option_g] [conv; i];
    (* Nested application: fine *)
    case [i, applied_option_g unit_g] [conv; i];
    ()
end

let () =
  Subst_tests.run ();
  Check_tests.run ()
