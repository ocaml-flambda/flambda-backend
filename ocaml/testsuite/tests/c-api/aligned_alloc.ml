(* TEST
  modules = "aligned_alloc_stubs.c"
  * runtime4
  ** skip
  * runtime5
  ** native
*)

(* CR ocaml 5 all-runtime5: replace with the version from stdlib *)
external make_contended : 'a -> 'a Atomic.t = "caml_atomic_make_contended"
external is_aligned : 'a Atomic.t -> bool = "caml_atomic_is_aligned"
let test_is_aligned () =
  let l = List.init 100 Atomic.make in
  let all_aligned =
    List.for_all is_aligned l
  in
  assert (not all_aligned)
;;

let test_make_contended () =
  let l = List.init 100 make_contended in
  List.iteri (fun i atomic ->
    assert (Atomic.get atomic == i);
    assert (is_aligned atomic)) l
;;

let () =
  test_is_aligned ();
  test_make_contended ();
;;