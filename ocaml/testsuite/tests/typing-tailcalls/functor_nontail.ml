(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-no-always-tco -dlambda -dno-unique-ids";
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

module Make (M : sig
    type t

    val to_string : t -> string

    module Nested : sig
      val of_string : string -> t
    end
  end, S) =
struct
  module Test_open_M = struct
    open M

    let calls_to_string t = to_string t

    (* Test what happens when to_string is rebound to another identifier *)
    let to_string_rebound t = to_string t
    let calls_to_string_rebound t = to_string_rebound t
    let to_string_pointfree_rebound = to_string
    let calls_to_string_pointfree_rebound t = to_string_rebound t
  end

  module Test_open_Nested = struct
    open M.Nested

    let calls_of_string str = of_string str

    (* Test what happens when of_string is rebound to another identifier *)
    let of_string_rebound str = of_string str
    let calls_of_string_rebound str = of_string_rebound str
    let of_string_pointfree_rebound = of_string
    let calls_of_string_pointfree_rebound str = of_string_rebound str
  end

  (* M.to_string tests *)
  let to_string t = M.to_string t
  let calls_to_string t = to_string t
  let to_string_pointfree = M.to_string
  let calls_to_string_pointfree t = to_string_pointfree t

  (* Test what happens when to_string is rebound to another identifier *)
  let to_string_rebound t = to_string t
  let calls_to_string_rebound t = to_string_rebound t
  let to_string_pointfree_rebound = to_string
  let calls_to_string_pointfree_rebound t = to_string_rebound t

  (* M.nested.of_string tests *)
  let of_string str = M.Nested.of_string str
  let calls_of_string str = of_string str
  let of_string_pointfree = M.Nested.of_string
  let calls_of_string_pointfree str = of_string_pointfree str

  (* Test what happens when of_string is rebound to another identifier *)
  let of_string_rebound str = of_string str
  let calls_of_string_rebound str = of_string_rebound str
  let of_string_pointfree_rebound = of_string
  let calls_of_string_pointfree_rebound str = of_string_rebound str
end
