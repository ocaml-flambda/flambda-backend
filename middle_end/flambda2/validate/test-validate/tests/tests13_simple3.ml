external opaque : 'a -> 'a = "%opaque"

external ignore : 'a -> unit = "%ignore"

external ( + ) : int -> int -> int = "%addint"

(* If the .mli is deleted, [bar] shouldn't be deleted either, as it doesn't
   satisfy the check on the shape of the code age relation (see
   [Simplify_common]). *)

let[@inline always] bar i map_foo =
  ();
  fun () ->
    let j = i + 3 in
    ignore (opaque j);
    (map_foo [@inlined never]) (fun x -> x) ()

let rec map_foo f () =
    let g =
      if opaque false then bar 10 map_foo else bar 20 map_foo
    in
    let g_result = (g [@inlined always]) () in
    ignore (opaque g_result);
    ()
