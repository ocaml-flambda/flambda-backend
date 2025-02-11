let[@inline never] [@local never] [@specialize never] add_arrays_unrolled_manually
    a b c n =
  for i = 0 to (n / 2) - 1 do
    Array.unsafe_set c (i * 2)
      (Array.unsafe_get a (i * 2) + Array.unsafe_get b (i * 2));
    Array.unsafe_set c
      ((i * 2) + 1)
      (Array.unsafe_get a ((i * 2) + 1) + Array.unsafe_get b ((i * 2) + 1))
  done;
  if Int.rem n 2 = 1
  then
    Array.unsafe_set c (n - 1)
      (Array.unsafe_get a (n - 1) + Array.unsafe_get b (n - 1))

(* Currently won't be vectorized. Can vectorize it but it's not worth it
   according to our cost model. It will be vectorized when we add vectors beyond
   128 or arrays of elements smaller than 64-bit. *)
let[@inline never] [@local never] [@specialize never] initialize_array_const_unrolled_manually
    arr n =
  let i = ref 0 in
  while !i < n do
    Array.unsafe_set arr !i 0;
    Array.unsafe_set arr (!i + 1) 0;
    i := !i + 2
  done

(* Currently, won't be vectorized. If different groups can reuse the new
   register that holds the constants, this will be worth vectorizing even with
   128-bit vectors. *)
let[@inline never] [@local never] [@specialize never] initialize_arrays_const_unrolled_manually
    a b c n =
  let i = ref 0 in
  while !i < n do
    Array.unsafe_set a !i 0;
    Array.unsafe_set a (!i + 1) 0;
    Array.unsafe_set b !i 0;
    Array.unsafe_set b (!i + 1) 0;
    Array.unsafe_set c !i 0;
    Array.unsafe_set c (!i + 1) 0;
    i := !i + 2
  done

(* Currently, won't be vectorized. Shuffling values into a vector is not yet
   supported, only vector loads are. Also not worth it unless the shuffle is
   outside the loop (loop invariant detection/motion would be needed for it). *)
let[@inline never] [@local never] [@specialize never] initialize_array_unrolled_manually
    arr n (v : int) =
  let i = ref 0 in
  while !i < n do
    Array.unsafe_set arr !i v;
    Array.unsafe_set arr (!i + 1) v;
    i := !i + 2
  done

(* same as [initialize_array_unrolled_manually] except needs movddup. *)
let[@inline never] [@local never] [@specialize never] initialize_floatarray_unrolled_manually
    arr n (v : float) =
  let i = ref 0 in
  while !i < n do
    Array.unsafe_set arr !i v;
    Array.unsafe_set arr (!i + 1) v;
    i := !i + 2
  done

(* cannot vectorize across basic blocks *)
let[@inline never] [@local never] [@specialize never] add_arrays_unrolled_safe a
    b c n =
  for i = 0 to n - 1 do
    Array.set c (i * 2) (Array.get a (i * 2) + Array.get b (i * 2));
    Array.set c
      ((i * 2) + 1)
      (Array.get a ((i * 2) + 1) + Array.get b ((i * 2) + 1))
  done

(* cannot vectorize across basic blocks. unroll attribute is not sufficient to
   eliminate the loop condition from the unrolled body (e.g., we would need to
   track the fact that the bound is even. *)
let[@inline never] [@local never] [@specialize never] add_arrays_rec_unrolled_attribute
    a b c n =
  let[@loop never] rec loop i a b c n =
    if i < n
    then (
      Array.unsafe_set c i (Array.unsafe_get a i + Array.unsafe_get b i);
      (loop [@unrolled 1]) (i + 1) a b c n)
  in
  loop 0 a b c (2 * n)

(* cannot vectorize for-loops *)
let[@inline never] [@local never] [@specialize never] add_arrays_for a b c n =
  for i = 0 to n - 1 do
    Array.unsafe_set c i (Array.unsafe_get a i + Array.unsafe_get b i)
  done

(* cannot vectorize loops expressed using recursion *)
let[@inline never] [@local never] [@specialize never] add_arrays_rec a b c n =
  let rec loop i =
    if i < n
    then (
      Array.unsafe_set c i (Array.unsafe_get a i + Array.unsafe_get b i);
      loop (i + 1))
  in
  loop 0

let print_array ppf a =
  let count = Array.length a in
  for i = 0 to count - 1 do
    Format.fprintf ppf "%d " a.(i)
  done

let print_floatarray ppf a =
  let count = Array.length a in
  for i = 0 to count - 1 do
    Format.fprintf ppf "%f " a.(i)
  done

let () =
  let n = Sys.opaque_identity 10 in
  let a = Array.init n (fun i -> i) in
  let b = Array.make n 17 in
  let c = Array.make n 0 in
  let d = Array.make n 0.0 in
  add_arrays_unrolled_manually a b c (Sys.opaque_identity n);
  Format.printf "add_arrays_unrolled_manually %a\n" print_array c;
  add_arrays_unrolled_safe a b c (Sys.opaque_identity (n / 2));
  Format.printf "add_arrays_unrolled_safe %a\n" print_array c;
  add_arrays_rec_unrolled_attribute a b c (n / 2);
  Format.printf "add_arrays_rec_unrolled_attribute %a\n" print_array c;
  add_arrays_for a b c n;
  Format.printf "add_arrays_for %a\n" print_array c;
  add_arrays_rec a b c n;
  Format.printf "add_arrays_rec %a\n" print_array c;
  initialize_array_const_unrolled_manually c n;
  Format.printf "initialize_array_const_unrolled_manually %a\n" print_array c;
  initialize_arrays_const_unrolled_manually a b c n;
  Format.printf "initialize_arrays_const_unrolled_manually %a\n" print_array c;
  initialize_array_unrolled_manually c n (Sys.opaque_identity 17);
  Format.printf "initialize_array_unrolled_manually %a\n" print_array c;
  initialize_floatarray_unrolled_manually d n (Sys.opaque_identity 7.7);
  Format.printf "initialize_floatarray_unrolled_manually %a\n" print_floatarray
    d;
  ()
