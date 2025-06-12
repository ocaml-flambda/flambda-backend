(* TEST
 flambda;
 ocamlopt_flags = "-O3 -flambda2-inline-small-function-size 0";
 native;
*)

(* This test need to *not* be in classic mode to run,
   since we rely on the call to `loop` being inlined during
   the speculative inlining of `t`, which cannot happen in
   classic mode. *)
[@@@ocaml.flambda_o3]

external __opaque__ : 'a -> 'a = "%opaque"

let[@loop] rec loop () =
  (* The goal of the test is to have `t` be speculatively inlined,
     and during that speculative inlining, trigger a case where
     the call to `_loop` can be loopified and transformed into an
     apply_cont; if that happens during speculative inlining, then
     the Flow analaysis will raise an error because in the context
     of the inlined body, the `self` continuation is not
     defined/bound. *)
  let[@local never] t = (fun _ ->
    let _x = __opaque__ 0 in
    (loop[@inlined]) ()
    )
  in
  t ()

