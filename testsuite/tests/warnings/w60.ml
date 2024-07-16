(* TEST_BELOW
(* Blank lines added here to preserve locations. *)







*)

(* PR#7314 *)

module type Comparable = sig
  val id: int
end

module Make_graph (P:sig module Id:Comparable end) = struct
  let foo = P.Id.id
end

module Fold_ordered(P: sig module Id:Comparable end) =
struct
  include Make_graph(struct module Id = P.Id end)
end


(* PR#7314 *)

module M = struct
  module N = struct end
end

module O = M.N

(***************)

let () =
  (* M is unused, but no warning was emitted before 4.10. *)
  let module M = struct end in
  ()
<<<<<<< HEAD

(* Nominal type comparisons *)

module Nominal = struct
  module type S = sig
    module M : sig end
    type t
  end

  module M : S = struct
    module M = struct end
    type t = int
  end

  module F(X:S) = struct type t = X.t end

  module N = F(M)
end

(* TEST
 flags = "-w +A-67";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
||||||| 121bedcfd2
=======

(* TEST
 flags = "-w +A-67";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
>>>>>>> 5.2.0
