(* Immutable arrays *)

let a_iarray : int iarray = [: 1; 2; 3; 4; 5 :]

let b_iarray : float iarray = [: 1.; 2.; 3.; 4.; 5. :]

(* Mutable arrays (should not be counted) *)

let c_array : int array = [| 1; 2; 3; 4; 5 |]

let d_array : float array = [| 1.; 2.; 3.; 4.; 5. |]

let e_array : bool array = [| true; false; true |]
