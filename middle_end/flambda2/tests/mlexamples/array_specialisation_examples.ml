(* Generic get primitive, on an array of Values. Should be simplified to
   a Values primitive. *)
let f (arr : int option array) =
  let[@local] get a = Array.get a 0 in
  get arr

(* Same as above, with the additional length information that removes the
   bounds check. *)
let g (x : int option) =
  let[@local] get a = Array.get a 0 in
  get [| x |]

(* Example of a specialised primitive (on Immediates arrays) being called on a
   generic array. The specialised primitive should be kept. *)
let h (x : int) =
  let[@local] make v = [| v |] in
  let[@local] get (a : int array) = a.(0) in
  let a = make x in
  get a

(* Same as above, but with a GADT to ensure that the array creation is truly
   generic. *)
type _ wit =
  | Int : int wit
  | Other : _ wit

let i (type a) (w : a wit) (x : a) : int =
  let a : a array = [| x |] in
  match w with Int -> a.(0) | Other -> 0
