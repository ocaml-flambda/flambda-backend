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
