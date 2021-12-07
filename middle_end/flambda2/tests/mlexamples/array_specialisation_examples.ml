(* Should be simplified to a Values array load, but isn't because of the missing
   representation for Values arrays in Flambda kinds *)
let f (arr : int option array) =
  let[@local] get a = Array.get a 0 in
  get arr

(* Same as above, but the Make_array primitive computes a reasonable type for
   the array so the array load is correctly simplified to a Values load *)
let g (x : int option) =
  let[@local] get a = Array.get a 0 in
  get [| x |]

(* Example of a specialised primitive (on Immediates arrays) being called on an
   array of type Values. With the correct information from kinds, we could have
   refined the type of [a] and the array load primitive would have seen an
   argument of kind Immediates. The simplification of [Is_boxed_float x] means
   that we know we're not dealing with a float array, at least. *)
let h (x : int) =
  let[@local] make v = [| v |] in
  let[@local] get (a : int array) = a.(0) in
  let a = make x in
  get a

(* Same as above, but the use of a GADT means that even with the right handling
   of array kinds the type of [a] at the load point would still be a generic
   array. *)
type _ wit =
  | Int : int wit
  | Other : _ wit

let i (type a) (w : a wit) (x : a) : int =
  let a : a array = [| x |] in
  match w with Int -> a.(0) | Other -> 0
