(* TEST
 {
   setup-ocamlopt.opt-build-env;
   flags = "-no-always-tco -regalloc cfg -regalloc-param IRC_SPILLING_HEURISTICS:flat-uses -cfg-analyze-tailcalls -dcfg-tailcalls -c";
   compiler_reference2 = "${test_source_directory}/dcfg_tailcalls.dot";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
 }
 {
   setup-ocamlopt.opt-build-env;
   flags = "-no-always-tco -dcfg -c";
   compiler_reference2 = "${test_source_directory}/dcfg_tailcalls.compilers.reference";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
 }
*)

let rec normal_recursive n =
  if n > 0 then normal_recursive (n - 2) else ()

let calls_normal_recursive () = normal_recursive 3


let rec mutually_recursive0 n =
  if n > 0 then mutally_recursive1 (n - 2) else ()
and mutally_recursive1 n = mutually_recursive0 (n + 1)

let calls_mutually_recursive () = mutually_recursive0 3


let rec mutually_recursive_not_in_tail_position0 n =
  if n > 0 then mutally_recursive1 (n - 2) + 1 else n
and mutally_recursive1 n = mutually_recursive_not_in_tail_position0 (n + 1)

let calls_mutually_recursive_not_in_tail_position () =
  mutually_recursive_not_in_tail_position0 3


let [@inline never] multiple_callers n = n * 5
let calls_multiple_callers0 () = multiple_callers 0
let calls_multiple_callers1 () = let k = multiple_callers 1 in k + 1
let calls_multiple_callers2 () = multiple_callers 2 [@tail]


let [@loop never] rec explicit_tail n =
  if n > 0 then explicit_tail (n - 2) [@tail] else ()

let [@loop never] rec hint_tail n =
  if n > 0 then hint_tail (n - 2) [@tail hint] else ()

let [@loop never] rec explicit_nontail n =
  if n > 0 then explicit_nontail (n - 2) [@nontail] else ()

let [@loop never] rec nontail_optimized_to_tail n =
  if n > 0 then
    let result = nontail_optimized_to_tail (n - 2) in result
  else ()


(* Test helper recursion *)
let rec collatz n k =
  let[@inline never] next n k =
    let n = n - 1
    and k = if k mod 2 == 0 then (k / 2) else ((3 * k) + 1) in
    collatz n k
  in
  ignore next;
  if n > 0 then next n k else k


let rec mutually_recursive_and_calls_parameter0 ~f n =
  if n > 0 then mutually_recursive_and_calls_parameter1 ~f (n - 2) else f true
and mutually_recursive_and_calls_parameter1 ~f n =
  if n > 0 then mutually_recursive_and_calls_parameter0 ~f (n + 1) else f false


let calls_list_map_in_tail_position lst = List.map (fun x -> x + 1) lst
let calls_list_map_not_in_tail_position lst =
  let lst = List.map (fun x -> x + 1) lst in
  Sys.opaque_identity lst


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

let call_cps_style k = 
  cps_style0
    3
    (* Danger is that the continuation can reference the cps_style0 *)
    (fun x -> if x > 10 then cps_style0 x k else 0)


let [@loop never] rec apply_in_tail_position_in_exclave n =
  if n < 0 then n
  else exclave_ apply_in_tail_position_in_exclave (n - 2)


type int_option = None | Some of int

let bind f = function
  | None -> None
  | Some x -> f x

let rec monadic_loop int_option =
  let next x = if x < 0 then None else monadic_loop (Some (x - 2)) in
  bind next int_option
