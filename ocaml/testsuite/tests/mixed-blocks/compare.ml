(* TEST
 flags = "-extension layouts_alpha";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

(* Polymorphic comparison raises if either argument is a mixed block.

   The one exception is [compare x x], wich always returns [0].
   (Note that [x = x] still raises for mixed blocks!) This discrepancy
   between [compare] and [equal] is consistent with e.g. functional values.
*)

type forget = T : _ -> forget

let compare (x : forget) (y : forget) =
 match compare (T x) (T y) with
 | exception exn -> Printf.sprintf "raised %s" (Printexc.to_string exn)
 | i -> string_of_int i

let equal (x : forget) (y : forget) =
  match T x = T y with
  | exception exn -> Printf.sprintf "raised %s" (Printexc.to_string exn)
  | b -> string_of_bool b

type normal =
  { x : int;
    y : int;
  }

type all_float =
  { x : float;
    y : float#;
  }

type mixed1 =
  { x : string;
    y : float#;
    z : int;
  }

type mixed2 =
  { x : float#;
    y : int;
  }

let mixed =
  [ "all_float", T ({ x = 4.0; y = #5.0 } : all_float);
    "mixed1", T ({ x = "str"; y = #5.0; z = 3 } : mixed1);
    "mixed2", T ({ x = #3.0; y = 3 } : mixed2);
  ]

let normal =
  [ "normal", T ({ x = 4; y = 5 } : normal);
    "int", T 3;
    "string", T "string";
    "floatarray", T [| 5.0 |];
    "empty", T [||];
    "closure", T (let y = #5.0 in fun x -> x +. Stdlib__Float_u.to_float y);
  ]

let () = Printf.printf "NORMAL VS. MIXED\n\n"

let run_compare (label1, x1) (label2, x2) =
  Printf.printf "compare %-10s %-11s= %s\n"
    label1 label2 (compare x1 x2)

let run_equal (label1, x1) (label2, x2) =
  Printf.printf "%-10s = %-11s= %s\n"
    label1 label2 (equal x1 x2)

open StdLabels

let () =
  List.iter mixed ~f:(fun mixed ->
    List.iter normal ~f:(fun normal ->
      run_compare mixed normal);
    List.iter normal ~f:(fun normal ->
      run_compare normal mixed));
  List.iter mixed ~f:(fun mixed ->
    List.iter normal ~f:(fun normal ->
      run_equal mixed normal);
    List.iter normal ~f:(fun normal ->
      run_equal normal mixed));
  print_newline ()
;;

let () = Printf.printf "MIXED VS. MIXED\n\n"

let () =
  List.iter mixed ~f:(fun mixed1 ->
    List.iter mixed ~f:(fun mixed2 ->
      run_compare mixed1 mixed2));
  List.iter mixed ~f:(fun mixed1 ->
    List.iter mixed ~f:(fun mixed2 ->
      run_equal mixed1 mixed2));
;;
