(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-no-always-tco -dtypedtree -dlambda -dno-unique-ids";
 ocamlopt.opt;
 {
   stack-allocation;
   compiler_reference2 = "${test_source_directory}/functor_nontail.stack.reference";
   check-ocamlopt.opt-output;
 }{
   no-stack-allocation;
   compiler_reference2 = "${test_source_directory}/functor_nontail.heap.reference";
   check-ocamlopt.opt-output;
 }
*)

(* All the `apply_mode`'s in the generated typedtree are expected to be
   `apply_mode Nontail.

   All the `apply`'s in the generated lambda are expected to be `applynontail`. *)

module Make (M : sig
    type t

    val to_string : t -> string

    module Nested : sig
      val of_string : string -> t
    end
  end) =
struct
  module Test_open_M = struct
    open M
    (* We can't see that rebound functions came from the functor, so they
       are inferred as nontail. *)
    let to_string_rebound = to_string
    let calls_to_string_rebound t = to_string_rebound t
  end

  module Test_open_Nested = struct
    open M.Nested

    let of_string_rebound = of_string
    let calls_of_string_rebound str = of_string_rebound str
  end

  let to_string_rebound = M.to_string
  let calls_to_string_rebound t = to_string_rebound t

  let of_string_rebound = M.Nested.of_string
  let calls_of_string_rebound str = of_string_rebound str
end
