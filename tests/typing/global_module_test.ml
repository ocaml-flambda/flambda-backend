module Test_data = struct
  open Global_module

  open struct
    let args_of_pairs pairs =
      List.map (fun (param, value) -> { Argument.param; value }) pairs

    let n ?(args = []) head : Name.t = Name.create_exn head (args_of_pairs args)

    let g ?(vis = []) ?(hid = []) head =
      create_exn head (args_of_pairs vis) ~hidden_args:(args_of_pairs hid)
  end

  let [@ocamlformat "disable"] () =
    (* This [let] is only here so that ocamlformat lets us add line breaks in
       the following. Note that, despite being disabled, ocamlformat insists on
       adding whitespace errors by indenting empty lines. *)

    (* Here we imagine the following modules and parameters, with their
       parameters given in square brackets:

       {v
         - X (parameter)
         - Y[X] (parameter)
         - A : X
         - B : Y[X:A]
         - M[X][Y[X]] (regular module)
         - I (parameter)
         - O (parameter)
         - Conv[I][O] (parameter)
         - Unit : I or O
         - String : I or O
         - Opaque[I] : Conv[I][O:String]
         - Print[I][Conv[I][O:String]] (regular module)
       v}

       Each [*_p] is an [I.param] and an untagged identifier is an [I.t].
       *)
    ()

  let x = n "X"

  let x_g = g "X"

  let y = n "Y"

  let y_g = g "Y" ~hid:[x, x_g]

  let a_g = g "A"

  let b_g = g "B"

  let m_g = g "M" ~hid:[x, x_g; y, y_g]

  let i = n "I"

  let i_g = g "I"

  let o = n "O"

  let o_g = g "O"

  let conv = n "Conv"

  let conv_g = g "Conv" ~hid:[i, i_g; o, o_g]

  let unit_g = g "Unit"

  let string = n "String"

  let string_g = g "String"

  let conv_to_string = n "Conv" ~args:[o, string]

  let conv_to_string_g = g "Conv" ~vis:[o, string_g] ~hid:[i, i_g]

  let opaque_g = g "Opaque" ~hid:[i, i_g]

  let print_g = g "Print" ~hid:[i, i_g; conv_to_string, conv_to_string_g]
end

module Subst_tests = struct
  open struct
    let subst param s =
      Global_module.subst param (s |> Global_module.Name.Map.of_list)

    let [@ocamlformat "disable"] case glob s =
      let s = s |> Global_module.Name.Map.of_list in
      let s', _changed = Global_module.subst glob s in
      Format.printf "@[<hv 2>%a@ %a@ =@ %a@]@."
        Global_module.print glob
        (Global_module.Name.Map.print Global_module.print) s
        Global_module.print s'
  end

  let run () =
    Format.printf "@.Tests of subst:@.@.";
    let open Test_data in
    case x_g [x, unit_g];
    case y_g [x, unit_g];
    let y_unit, _changed = subst y_g [x, unit_g] in
    case y_unit [x, string_g];
    case y_unit [Global_module.to_name y_unit, string_g];
    case m_g [x, a_g];
    case m_g [x, a_g; y, b_g];
    case x_g [i, unit_g];
    case y_g [i, unit_g];
    case print_g [i, unit_g];
    ()
end

module Check_tests = struct
  open struct
    let print_params ppf params =
      let pp_list_body ppf params =
        Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
          Global_module.print ppf params
      in
      match params with
      | [] -> Format.fprintf ppf "[]"
      | _ :: _ -> Format.fprintf ppf "@[<hov 2>[@ %a@ ]@]" pp_list_body params

    let [@ocamlformat "disable"] case s params =
      let s = s |> Global_module.Name.Map.of_list in
      Format.printf "@[<hv 2>@[<hv 2>check@ %a@ %a@]@ = %a@]@."
        (Global_module.Name.Map.print Global_module.print) s
        print_params params
        Format.pp_print_bool (Global_module.check s params)
  end

  let run () =
    Format.printf "@.Tests of check:@.@.";
    let open Test_data in
    (* Trivial case: fine *)
    case [] [];
    (* Fully unapplied: fine *)
    case [] [x_g];
    (* Argument given but no parameters: bad *)
    case [x, string_g] [];
    (* Exactly one argument and one matching parameter: fine *)
    case [x, string_g] [x_g];
    (* Exactly one argument and one matching parameter: fine *)
    case [i, unit_g] [i_g];
    (* Exactly one argument and one non-matching parameter: bad *)
    case [i, unit_g] [o_g];
    (* Partial application: fine *)
    case [i, unit_g] [conv_g; i_g];
    (* Partial application of parameterised parameter: fine *)
    case [conv, opaque_g; o, string_g] [conv_g; i_g; o_g];
    (* Partial application with too-specialised argument: bad *)
    case [conv_to_string, opaque_g] [conv_g; i_g; o_g];
    (* As previously, but after [String] is substituted for [O]: fine *)
    case [conv_to_string, opaque_g] [conv_to_string_g; i_g];
    ()
end

let () =
  Subst_tests.run ();
  Check_tests.run ()
