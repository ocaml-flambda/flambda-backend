(* Differentiates between optimisation levels. Requires inspecting -dcmm to
   verify.

   Three values are exported by the module. Whether they are compile-time
   constants depends on the flags in effect: *)

(* - With -Oclassic, none are constant. *)
(* - With no flags at all, x is constant. *)
(* - With -O2, x and y are constant. *)
(* - With -O3, all three are constant. *)

(* Except in -Oclassic, which doesn't statically allocate [M] at all, the -dcmm
   output should have a block with just a few integers near the top; they'll
   either be the computed constant in tagged form or 1 if the value isn't
   constant at that level. In particular, they should be: *)

(* - (no flags): 85 1 1
 * - -O2: 85 199 1
 * - -O3: 85 199 3403 *)

module M : sig
  val x : int

  val y : int

  val z : int
end = struct
  let f () =
    let g () = 42 in
    g, g

  (* requires -Oclassic off *)
  let x =
    (* if f appears only once, -Oclassic inlines it *)
    let _ = f in
    let f, _ = f () in
    f ()

  external getenv : string -> string = "caml_sys_getenv"

  (* requires -O2 or -O3 (or -flambda2-join-points) *)
  let y =
    match match getenv "foo" with _ -> Some 1 | exception _ -> Some 2 with
    | Some _ -> 99
    | None -> -1

  module F (X : sig
    val g : unit -> int
  end) =
  struct
    let h () = X.g ()
  end
  [@@inline never]

  module X = struct
    let g () = 1701
  end

  module FX = F (X)

  (* requires -O3 (or any -flambda2-result-types-X) *)
  let z = FX.h ()
end
