(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-no-always-tco -regalloc cfg -regalloc-param IRC_SPILLING_HEURISTICS:flat-uses -cfg-analyze-tailcalls";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(* Test helper recursion *)
let rec collatz n k =
  let[@inline never] next n k =
    let n = n - 1
    and k = if k mod 2 == 0 then (k / 2) else ((3 * k) + 1) in
    collatz n k
  in
  ignore next;
  if n > 0 then next n k else k


(* CPS style is OK by itself. We may warn on the function that calls
   into CPS code (`call_cps_style`). *)
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


type int_option = None | Some of int

let[@inline never] bind f = function
  | None -> None
  | Some x -> f x

let rec monadic_loop int_option =
  let next x = if x < 0 then None else monadic_loop (Some (x - 2)) in
  bind next int_option


let rec long_known_cycle () =
  let[@inline never] bar () = long_known_cycle () [@tail] in
  let[@inline never] baz () = bar () [@tail] in
  let[@inline never] quux () = baz () [@tail] in
  let[@inline never] wibble () = quux () [@tail] in
  let[@inline never] nibble () = wibble () [@tail] in
  ignore bar; ignore baz; ignore quux; ignore wibble; ignore nibble;
  nibble ()

let short_unknown_cycle () =
  let unknown = (Sys.opaque_identity (fun () -> ()))
  in unknown ()

(* The warning should prefer a longer cycle that has no unknown
   vertex than a shorter one that has an unknown vertex. Two candidate
   cycles here are:

   unknown () -> nibble () -> unknown ()
                 ^^^^^^^^^

      prefer_long_known_cycle b ()
   -> nibble ()
      ^^^^^^^^^
   -> wibble ()
   -> quux ()
   -> baz ()
   -> bar ()
   -> prefer_long_known_cycle b ()

   We would like to display the second cycle.
*)
let rec prefer_long_known_cycle b () =
  let[@inline never] bar () = prefer_long_known_cycle b () [@tail] in
  let[@inline never] baz () = bar () [@tail] in
  let[@inline never] quux () = baz () [@tail] in
  let[@inline never] wibble () = quux () [@tail] in
  let[@inline never] nibble () =
    if b then wibble () [@tail]
    else
      let unknown = (Sys.opaque_identity (fun () -> ()))
      in unknown () [@tail]
  in
  ignore bar; ignore baz; ignore quux; ignore wibble; ignore nibble;
  nibble ()
