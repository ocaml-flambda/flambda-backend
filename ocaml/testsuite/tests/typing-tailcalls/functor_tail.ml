(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-no-always-tco -dtypedtree -dlambda -dno-unique-ids";
 ocamlopt.opt;
 {
   stack-allocation;
   compiler_reference2 = "${test_source_directory}/functor_tail.stack.reference";
   check-ocamlopt.opt-output;
 }{
   no-stack-allocation;
   compiler_reference2 = "${test_source_directory}/functor_tail.heap.reference";
   check-ocamlopt.opt-output;
 }
*)

(* All the `apply_mode`'s in the generated typedtree are expected to be
   `apply_mode Tail.

   All the `apply`'s in the generated lambda are expected to be `apply`
   (and NOT `applynontail`). *)

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

    (* This call should show up as `M.to_string` in the parsetree. *)
    let calls_to_string t = to_string t
  end

  module Test_open_Nested = struct
    open M.Nested

    let calls_of_string str = of_string str
  end

  let to_string t = M.to_string t
  let of_string str = M.Nested.of_string str
end

