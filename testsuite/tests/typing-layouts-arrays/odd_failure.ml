(* TEST
 include stdlib_stable;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_beta";
 native;
*)

external[@layout_poly] makearray_dynamic_local
  : ('a : any_non_null) . int -> 'a -> 'a array @ local
  = "%makearray_dynamic"

external[@layout_poly] get
  : ('a : any_non_null) . ('a array[@local_opt]) -> (int[@local_opt]) -> 'a
  = "%array_safe_get"

external[@layout_poly] set
  : ('a : any_non_null) . ('a array[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
  = "%array_safe_set"

external opaque_identity : ('a[@local_opt]) -> ('a[@local_opt]) = "%opaque"

let r : unit list ref = ref []

let () =
  let arr = makearray_dynamic_local 10 #(0, 0L) in
  let seven = opaque_identity 7 in
  let seven' = opaque_identity 7 in
  set arr 0 #(0, Int64.of_int seven);
  Gc.compact ();
  set arr 0 #(0, Int64.of_int seven');
  Gc.compact ();
  for _ = 0 to 0 do
    r := () :: !r;
    let #(_, x) = get arr 0 in
    let x = Int64.to_int x in
    Printf.printf "%d\n" x; (* should be 7, but prints 1 *)
    assert (Int.equal x 7)
  done
;;
