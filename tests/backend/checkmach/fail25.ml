(* all functions fail the check when -disable-checkmach is passed. *)
module Sexp = struct
  type t =
    | Atom of string
    | List of t list
  let of_int n = Atom (string_of_int n)
end

exception Exn of Sexp.t

let raise_s sexp = raise (Exn sexp)
[@@ocaml.inline never][@@ocaml.local never][@@ocaml.specialise never]

(* Pattern before ppx looks like this: *)
(* let[@zero_alloc] raise_invalid_index ~unknown_index ~name =
 *   raise_s
 *     [%message
 *       "Unknown" (unknown_index : int) (name : string)]
 * ;; *)

(* pass the check when -disable-precise-checkmach is passed, because
   conservative summary of ppx_sexp_message is on used on a path to
   raise_s. *)
let raise_invalid_index_FAIL ~unknown_index ~name =
  raise_s
    (let ppx_sexp_message () =
       Sexp.List
         [Sexp.Atom "Unknown";
          Sexp.List
            [Sexp.Atom "unknown_index"; Sexp.of_int unknown_index];
          Sexp.List
            [Sexp.Atom "name"; Sexp.Atom name]]
     [@@ocaml.inline never][@@ocaml.local never][@@ocaml.specialise never]
     in
     ((ppx_sexp_message ())[@nontail ]))
[@@ocaml.inline never][@@ocaml.local never][@@ocaml.specialise never][@@zero_alloc ]

(* Always fails the check, even with precise analysis of recursive functions. *)
let[@zero_alloc] rec g x =
  if Sys.opaque_identity true then
    try g x with _ -> ()
  else raise (Failure x)

(* Functions below fail the check when -disable-precise-checkmach is passed. *)
let[@zero_alloc] rec foo n =
  bar (n-1)
and[@zero_alloc] bar n =
  foo (n-1)

let[@zero_alloc] rec f1 x =
  if x > Sys.opaque_identity 10 then x else
  f2 (x + 1)
and[@zero_alloc] f2 x =
  f1 (x + 1)

(* Fail the check when -disable-precise-checkmach is passed and -function-layout source *)
let[@zero_alloc] outer x =
  let[@zero_alloc][@inline never][@local never] inner x =
    if x > Sys.opaque_identity 10
    then x
    else (x + 1)
  in
  inner (x+1)
