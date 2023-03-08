module String_id = struct
  type t = string

  include Identifiable.Make (struct
    type nonrec t = t

    let equal = String.equal

    let compare = String.compare

    let print = Format.pp_print_string

    let output out s = Printf.fprintf out "%s" s

    let hash = Hashtbl.hash
  end)
end

module I = Instance.Make (String_id)

module Test_data = struct
  open I

  open struct
    (* Simple name *)
    let s name = { head = name; args = [] }

    (* Simple param *)
    let p name : I.param = { head = name; args = []; params = [] }
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

  let x = s "X"

  let x_p = p "X"

  let y = s "Y"

  let y_p = { (p "Y") with params = [x, x_p] }

  let a_p = p "A"

  let b_p = p "B"

  let m_p = { (p "M") with params = [x, x_p; y, y_p] }

  let i = s "I"

  let i_p = p "I"

  let o = s "O"

  let o_p = p "O"

  let conv = s "Conv"

  let conv_p = { (p "Conv") with params = [i, i_p; o, o_p] }

  let unit_p = p "Unit"

  let string = s "String"

  let string_p = p "String"

  let conv_to_string = { head = "Conv"; args = [o, string] }

  let conv_to_string_p =
    { head = "Conv"; args = [o, string_p]; params = [i, i_p] }

  let opaque_p = { (p "Opaque") with params = [i, i_p] }

  let print_p =
    { (p "Print") with params = [i, i_p; conv_to_string, conv_to_string_p] }
end

module Subst_tests = struct
  open struct
    let subst param s = I.subst param (s |> I.Map.of_list)

    let [@ocamlformat "disable"] case param s =
      let s = s |> I.Map.of_list in
      Format.printf "@[<hv 2>%a@ %a@ =@ %a@]@."
        I.print_param param
        (I.Map.print I.print_param) s
        I.print_param (I.subst param s)
  end

  let run () =
    Format.printf "@.Tests of subst:@.@.";
    let open Test_data in
    case x_p [x, unit_p];
    case y_p [x, unit_p];
    let y_unit = subst y_p [x, unit_p] in
    case y_unit [x, string_p];
    case y_unit [I.erase_param y_unit, string_p];
    case m_p [x, a_p];
    case m_p [x, a_p; y, b_p];
    case x_p [i, unit_p];
    case y_p [i, unit_p];
    case print_p [i, unit_p];
    ()
end

module Check_tests = struct
  open struct
    let print_params ppf params =
      let pp_list_body ppf params =
        Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
          I.print_param ppf params
      in
      match params with
      | [] -> Format.fprintf ppf "[]"
      | _ :: _ -> Format.fprintf ppf "@[<hov 2>[@ %a@ ]@]" pp_list_body params

    let [@ocamlformat "disable"] case s params =
      let s = s |> I.Map.of_list in
      Format.printf "@[<hv 2>@[<hv 2>check@ %a@ %a@]@ = %a@]@."
        (I.Map.print I.print_param) s
        print_params params
        Format.pp_print_bool (I.check s params)
  end

  let run () =
    Format.printf "@.Tests of check:@.@.";
    let open Test_data in
    (* Trivial case: fine *)
    case [] [];
    (* Fully unapplied: fine *)
    case [] [x_p];
    (* Argument given but no parameters: bad *)
    case [x, string_p] [];
    (* Exactly one argument and one matching parameter: fine *)
    case [x, string_p] [x_p];
    (* Exactly one argument and one matching parameter: fine *)
    case [i, unit_p] [i_p];
    (* Exactly one argument and one non-matching parameter: bad *)
    case [i, unit_p] [o_p];
    (* Partial application: fine *)
    case [i, unit_p] [conv_p; i_p];
    (* Partial application of parameterised parameter: fine *)
    case [conv, opaque_p; o, string_p] [conv_p; i_p; o_p];
    (* Partial application with too-specialised argument: bad *)
    case [conv_to_string, opaque_p] [conv_p; i_p; o_p];
    (* As previously, but after [String] is substituted for [O]: fine *)
    case [conv_to_string, opaque_p] [conv_to_string_p; i_p];
    ()
end

let () =
  Subst_tests.run ();
  Check_tests.run ()
