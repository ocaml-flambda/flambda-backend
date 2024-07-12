(* TEST
 flags = "-g -extension layouts_alpha";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)


(* type 'a lst = Nil | Cons of 'a * 'a lst

type t : value => value = lst

type a = int t *)

type t = list list

(* module type M = sig
  type r : (value => value) => value
  type s : value => value
  val g : list s r -> list s r
end *)
