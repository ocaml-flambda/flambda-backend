open Stdlib_upstream_compatible

external[@layout_poly] unsafe_set : ('a : any_non_null). 'a array -> int -> 'a -> unit =
  "%array_unsafe_set"

module I64 = struct
  type elt = int64#

  type t = elt array

  external[@layout_poly] reinterp_get :
    ('a : any). t -> int -> 'a = "%magic_reinterp_array_unsafe_get"

  external[@layout_poly] reinterp_set :
    ('a : any). t -> int -> 'a -> unit = "%magic_reinterp_array_unsafe_set"

  external create_uninitialized
    :  len:int -> t
    = "caml_make_unboxed_int64_vect_bytecode" "caml_make_unboxed_int64_vect"

  let init len ~f =
    let r = create_uninitialized ~len in
    for i = 0 to len - 1 do
      unsafe_set r i (f i)
    done;
    r
  ;;

  let a_i64 = init 20 ~f:(fun i -> Int64_u.of_int i)

  let _ =
    Printf.printf "\nint64# array\n";
    let #(i4, i5, i6) : #(elt * elt * elt) = reinterp_get a_i64 4 in
    Printf.printf "expect #(4, 5, 6): #(%d, %d, %d)\n"
      (Int64_u.to_int i4) (Int64_u.to_int i5) (Int64_u.to_int i6);
    reinterp_set a_i64 6 #(Int64_u.of_int 101, Int64_u.of_int 102);
    let #(i5, i6, i7, i8) : #(elt * elt * elt * elt) = reinterp_get a_i64 5 in
    Printf.printf "expect #(5, 101, 102, 8): #(%d, %d, %d, %d)\n"
      (Int64_u.to_int i5) (Int64_u.to_int i6)
      (Int64_u.to_int i7) (Int64_u.to_int i8);
end

module Floatarray = struct
  external[@layout_poly] reinterp_get :
    ('a : any). floatarray -> int -> 'a = "%magic_reinterp_array_unsafe_get"

  external[@layout_poly] reinterp_set :
    ('a : any). floatarray -> int -> 'a -> unit = "%magic_reinterp_array_unsafe_set"

  let init len ~f =
    let r = Float.Array.create len in
    for i = 0 to len - 1 do
      Float.Array.unsafe_set r i (f i)
    done;
    r
  ;;

  let a_float = init 20 ~f:(fun i -> Float.of_int i)

  let _ =
    Printf.printf "\nfloatarray\n";
    let #(i4, i5, i6) : #(float# * float# * float#) = reinterp_get a_float 4 in
    Printf.printf "expect #(4.0, 5.0, 6.0): #(%.1f, %.1f, %.1f)\n"
      (Float_u.to_float i4) (Float_u.to_float i5) (Float_u.to_float i6)
end


module Floatuarray = struct
  external[@layout_poly] reinterp_get :
    ('a : any). float# array -> int -> 'a = "%magic_reinterp_array_unsafe_get"

  external[@layout_poly] reinterp_set :
    ('a : any). float# array -> int -> 'a -> unit = "%magic_reinterp_array_unsafe_set"

  external[@layout_poly] make_vect : ('a : any_non_null) . int -> 'a -> 'a array =
    "%makearray_dynamic"

  let init len ~f =
    let r = make_vect len (f 0) in
    for i = 0 to len - 1 do
      unsafe_set r i (f i)
    done;
    r
  ;;

  let a_float = init 20 ~f:(fun i -> Float_u.of_int i)

  let _ =
    Printf.printf "\nfloat# array\n";
    let #(i4, i5, i6) : #(float# * float# * float#) = reinterp_get a_float 4 in
    Printf.printf "expect #(4.0, 5.0, 6.0): #(%.1f, %.1f, %.1f)\n"
      (Float_u.to_float i4) (Float_u.to_float i5) (Float_u.to_float i6)
end

module Valuearray1 = struct
 external[@layout_poly] reinterp_get :
   ('a : any). string array -> int -> 'a = "%magic_reinterp_array_unsafe_get"

 external[@layout_poly] reinterp_set :
   ('a : any). string array -> int -> 'a -> unit =
   "%magic_reinterp_array_safe_set"

 let a_string = Array.init 20 (fun i -> Int.to_string i)

 let _ =
    Printf.printf "\nstring array (simple)\n";
    let #(i4, i5, i6) : #(string * string * string) = reinterp_get a_string 4 in
    Printf.printf "expect #(4, 5, 6): #(%s, %s, %s)\n" i4 i5 i6
end

module Valuearray2 = struct
 external[@layout_poly] reinterp_get :
   ('a : any). string array -> int -> 'a = "%magic_reinterp_array_unsafe_get"

 external[@layout_poly] reinterp_set :
   ('a : any). string array -> int -> 'a -> unit =
   "%magic_reinterp_array_unsafe_set"

 type t1 = A | B of int | C of string

 type t2 = D of float | E of float option

 let a_string =
   let a = Array.init 20 (fun _ -> "") in
   reinterp_set a 2 #(A, B 15, C "foo");
   reinterp_set a 5 #(D 17.0, E None);
   a

 let t1_to_string = function
   | A -> "A"
   | B i -> Printf.sprintf "B %d" i
   | C s -> Printf.sprintf "C %s" s

 let t2_to_string = function
   | D f -> Printf.sprintf "D %.1f" f
   | E None -> "E None"
   | E (Some f) -> Printf.sprintf "E %f" f

 let _ =
    let #(i0, i1, i2) : #(string * string * t1) = reinterp_get a_string 0 in
    Printf.printf "\nstring array (wonky)\n";
    Printf.printf "expect #(\"\", \"\", A): #(\"%s\", \"%s\", %s)\n"
      i0 i1 (t1_to_string i2);
    let #(i3, i4, i5) : #(t1 * t1 * t2) = reinterp_get a_string 3 in
    Printf.printf "expect #(B 15, C foo, D 17.0): #(%s, %s, %s)\n"
      (t1_to_string i3) (t1_to_string i4) (t2_to_string i5);
    let #(i6, i7) : #(t2 * string) = reinterp_get a_string 6 in
    Printf.printf "expect #(E None, \"\"): #(%s, \"%s\")\n"
      (t2_to_string i6) i7
end
