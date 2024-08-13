(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-no-always-tco -regalloc cfg -regalloc-param IRC_SPILLING_HEURISTICS:flat-uses -cfg-analyze-tailcalls -c";
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


(* External calls *)
external foo_ext : unit -> unit = "foo_ext"

let calls_foo_ext () = foo_ext ()

module Make (M : sig
    type t

    external to_string : t -> string = "to_string"

    module Nested : sig
      external of_string : string -> t = "of_string"
    end
  end) =
struct
  module Test_open_M = struct
    open M
    let calls_to_string t = to_string t
  end

  module Test_open_Nested = struct
    open M.Nested

    let calls_of_string str = of_string str
  end

  let to_string t = M.to_string t
  let of_string str = M.Nested.of_string str
end


(* With no optimization passes, this currently (falsely) warns that there is a
   cycle, since the partial application of `add` generates a closure which is
   called indirectly. Under higher optimization levels, the compiler is able
   to remove this indirect call. *)
let[@inline never] add a b = a + b

let foo k = add 1
