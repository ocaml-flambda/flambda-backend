(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-no-always-tco -regalloc cfg -regalloc-param IRC_SPILLING_HEURISTICS:flat-uses -cfg-analyze-tailcalls";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

let rec normal_recursive n =
  if n > 0 then normal_recursive (n - 2) else ()

let calls_normal_recursive () = normal_recursive 3


let rec mutually_recursive0 n =
  if n > 0 then mutally_recursive1 (n - 2) else ()
and mutally_recursive1 n = mutually_recursive0 (n + 1)


let calls_list_map_in_tail_position lst = List.map (fun x -> x + 1) lst
let calls_list_map_not_in_tail_position lst =
  let lst = List.map (fun x -> x + 1) lst in
  Sys.opaque_identity lst


let calls_mutually_recursive () = mutually_recursive0 3
let rec mutually_recursive_and_calls_parameter0 ~f n =
  if n > 0 then mutually_recursive_and_calls_parameter1 ~f (n - 2) else f true
and mutually_recursive_and_calls_parameter1 ~f n =
  if n > 0 then mutually_recursive_and_calls_parameter0 ~f (n + 1) else f false


let rec cps_style0 n k =
  if n < 0 then k n
  else
    let n = n - 10 in
    cps_style1 n k

and cps_style1 n k =
  let n = n + 1 in
  cps_style2 n k

and cps_style2 n k =
  let n = n + 5 in
  cps_style0 n k


let [@loop never] rec apply_in_tail_position_in_exclave n =
  if n < 0 then n
  else exclave_ apply_in_tail_position_in_exclave (n - 2)
