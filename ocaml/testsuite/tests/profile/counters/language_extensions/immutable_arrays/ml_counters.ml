(* Immutable arrays *)

(* 2 type, 1 expression *)
let a_iarray : int iarray = [: 1; 2; 3; 4; 5 :]

(* 2 type, 1 expression *)
let b_iarray : float iarray = [: 1.; 2.; 3.; 4.; 5. :]

(* 1 expression, 1 pattern *)
let and_iarray =
  match [: true; false :] with
  | [: iarray_val_1; iarray_val_2 :] -> iarray_val_1 && iarray_val_2
  | _ -> false

(* CR-someday mitom: Add tests for [Iarray] module from stdlib *)

(* Mutable arrays (should not be counted) *)

let c_array : int array = [| 1; 2; 3; 4; 5 |]

let d_array : float array = [| 1.; 2.; 3.; 4.; 5. |]

let e_array : bool array = [| true; false; true |]

let and_array =
  match [| true; false |] with
  | [| array_val_1; array_val_2 |] -> array_val_1 && array_val_2
  | _ -> false
