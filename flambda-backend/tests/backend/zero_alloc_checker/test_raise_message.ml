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



let ppx_sexp_message ~unknown_index ~name =
  Sexp.List
    [Sexp.Atom "Unknown";
     Sexp.List
       [Sexp.Atom "unknown_index"; Sexp.of_int unknown_index];
     Sexp.List
       [Sexp.Atom "name"; Sexp.Atom name]]
[@@ocaml.inline never][@@ocaml.local never][@@ocaml.specialise never]

let raise_invalid_index ~unknown_index ~name =
  raise_s ((ppx_sexp_message ~unknown_index ~name)[@nontail ])
[@@ocaml.inline never][@@ocaml.local never][@@ocaml.specialise never][@@zero_alloc ]
