(* TEST
 stack-allocation;
 native;
*)

(* Check whether functions that *could* take their argument
   locally allow their callers to locally construct the argument.

   Among other things, this checks how mode variables are defaulted in the
   presence of an mli.

   See the [..._no_mli.ml] version of this test for how mode variables
   are defaulted in the absence of an mli.
 *)

let check_if_zero_alloc =
  let measure_words f =
    let before = Gc.allocated_bytes () in
    ignore (f () : _);
    let after  = Gc.allocated_bytes () in
    int_of_float (after -. before) / (Sys.word_size / 8)
  in
  fun[@inline never] ~name ~f x ->
    let words = measure_words (fun () -> f x) - measure_words ignore in
    Printf.printf "%s: %d words%s\n" name words
      (if words = 0 then "" else " (allocates!)")

external escape : 'a -> 'a = "%identity"

type t = { a : int; b : int }

module _ : sig end = struct
  let[@inline never] take_unrestricted { a; b } = a + b

  let () =
    check_if_zero_alloc ~name:"take unrestricted of global (not exposed)" 0 ~f:(fun x ->
      take_unrestricted { a = x; b = x } [@nontail])
end

module _ : sig end = struct
  let[@inline never] take_unrestricted { a; b } = a + b

  let () =
    check_if_zero_alloc ~name:"take unrestricted of local (not exposed)" 0 ~f:(fun x ->
      take_unrestricted (local_ { a = x; b = x }) [@nontail])
end

module _ : sig end = struct
  let[@inline never] take_local (local_ { a; b }) = a + b

  let () =
    check_if_zero_alloc ~name:"take local of local (not exposed)" 0 ~f:(fun x ->
      take_local (local_ { a = x; b = x }) [@nontail])
end

module _ : sig end = struct
  let[@inline never] take_local (local_ { a; b }) = a + b

  let () =
    check_if_zero_alloc ~name:"take local of global (not exposed)" 0 ~f:(fun x ->
      take_local { a = x; b = x } [@nontail])
end

module _ : sig end = struct
  let[@inline never] take_global ({ a; b } as t) = ignore (escape t); a + b

  let () =
    check_if_zero_alloc
      ~name:"take global of global (not exposed; expected to allocate)"
      0
      ~f:(fun x -> take_global { a = x; b = x } [@nontail])
end

module M1 = struct
  let[@inline never] take_unrestricted { a; b } = a + b

  (* Note [Inference affects allocation in mli-less files]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

     At the moment, this allocates in the without-mli case, but not for a
     fundamental reason: when the type signature is inferred, the inferred
     types and thus mode variables are reused. Instead, we could create new
     copies of types with fresh mode variables related by submoding.
   *)
  let () =
    check_if_zero_alloc ~name:"take unrestricted of global (exposed)" 0 ~f:(fun x ->
      take_unrestricted { a = x; b = x } [@nontail])
end

module M2 = struct
  let[@inline never] take_unrestricted { a; b } = a + b

  let () =
    check_if_zero_alloc ~name:"take unrestricted of local (exposed)" 0 ~f:(fun x ->
      take_unrestricted (local_ { a = x; b = x }) [@nontail])
end

module M3 = struct
  let[@inline never] take_local (local_ { a; b }) = a + b

  let () =
    check_if_zero_alloc ~name:"take local of local (exposed)" 0 ~f:(fun x ->
      take_local (local_ { a = x; b = x }) [@nontail])
end

module M4 = struct
  let[@inline never] take_local (local_ { a; b }) = a + b

  let () =
    check_if_zero_alloc ~name:"take local of global (exposed)" 0 ~f:(fun x ->
      take_local { a = x; b = x } [@nontail])
end

module M5 = struct
  let[@inline never] take_global ({ a; b } as t) = ignore (escape t); a + b

  let () =
    check_if_zero_alloc
      ~name:"take global of global (exposed; expected to allocate)"
      0
      ~f:(fun x -> take_global { a = x; b = x } [@nontail])
end

module M6 = struct
  let[@inline never] take_local__global_in_mli (local_ { a; b }) = a + b

  let () =
    check_if_zero_alloc ~name:"take local of local (exposed)" 0 ~f:(fun x ->
      take_local__global_in_mli (local_ { a = x; b = x }) [@nontail])
end

module M7 = struct
  let[@inline never] take_local__global_in_mli (local_ { a; b }) = a + b

  let () =
    check_if_zero_alloc ~name:"take local of global (exposed)" 0 ~f:(fun x ->
      take_local__global_in_mli { a = x; b = x } [@nontail])
end
