(* TEST
 expect;
*)

(* An example where [type_argument] can refine modes using a gadt equation. *)
type _ t =
  | A : bool t

let id x = x

let f (type output) ~(output : output t) =
    match output with
    | A -> (id : bool -> output)
[%%expect{|
type _ t = A : bool t
val id : 'a -> 'a = <fun>
val f : output:'output t -> bool -> 'output = <fun>
|}]
