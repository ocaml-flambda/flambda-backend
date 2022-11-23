(* Lambda output:
 * (let (f/261 = (function a/263[addrarray] (array.get[gen] a/263 0)))
 *   (makeblock 0 f/261))
 *)
(* The [gen] annotation on array.get is expected: the [get] function is typed as
   'a array -> 'a, and it's only during simplification of local functions that
   its body is brought back into the body of [f], without changing the
   annotations. Regular inlining would have caused the same situation. *)

(* Expected Cmm output:
 * (function{tmp/array_kind.ml:1,6-61} camlArray_kind__f_0_1_code (a49/315: val)
 *  (if (<a 1 (>>u (load_mut int (+a a49/315 -8)) 9)) (load_mut val a49/315)
 *    (extcall "caml_ml_array_bound_error"{tmp/array_kind.ml:2,14-19} ->.)))
 *)
(* The check for the length is of course there, but the check for the float
   array tag has been removed thanks to the kind info on the argument. *)

let f (a : _ option array) =
  let get a = a.(0) in
  get a
