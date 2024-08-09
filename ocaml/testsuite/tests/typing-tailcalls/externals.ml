(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-no-always-tco -dtypedtree -dlambda -dno-unique-ids -c";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(* These should all be nontail. *)
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
