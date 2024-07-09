(* TEST
 flags = "-g -extension layouts_alpha";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

type t : value => value

(* [%%expect {|
type t : (value) => value
|}] *)

module M : sig
  type a : value => value
end = struct
  type a : value => value
end

(* [%%expect {|
module M : sig type a : (value) => value end
|}] *)

module M : sig
  type a : value => value
end = struct
  type a : value
end

(* [%%expect {|
|}] *)

module M : sig
  type a : value
end = struct
  type a : value => value
end

(* [%%expect {|
|}] *)
