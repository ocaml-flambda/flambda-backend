(* TEST
 readonly_files = "float_u_array.ml";
 modules = "${readonly_files}";
 include stdlib_upstream_compatible;
 flambda2;
 {
   native;
 }{
   bytecode;
 }{
   flags = "-extension layouts_beta";
   native;
 }{
   flags = "-extension layouts_beta";
   bytecode;
 }
*)
(* Test compilation correctness for array of unboxed floats. General
   tests around type-checking should go to [basics.ml]. *)

open Printf

(* This is the module type of [Float.Array] except type [t] is abstract. *)
module type S = sig
  type t
  val length : t -> int
  val get : t -> int -> float
  val set : t -> int -> float -> unit
  val make : int -> float -> t
  val create : int -> t
  val init : int -> (int -> float) -> t
  val append : t -> t -> t
  val concat : t list -> t
  val sub : t -> int -> int -> t
  val copy : t -> t
  val fill : t -> int -> int -> float -> unit
  val blit : t -> int -> t -> int -> int -> unit
  val to_list : t -> float list
  val of_list : float list -> t
  val iter : (float -> unit) -> t -> unit
  val iteri : (int -> float -> unit) -> t -> unit
  val map : (float -> float) -> t -> t
  val map_inplace : (float -> float) -> t -> unit
  val mapi : (int -> float -> float) -> t -> t
  val mapi_inplace : (int -> float -> float) -> t -> unit
  val fold_left : ('a -> float -> 'a) -> 'a -> t -> 'a
  val fold_right : (float -> 'a -> 'a) -> t -> 'a -> 'a
  val iter2 : (float -> float -> unit) -> t -> t -> unit
  val map2 : (float -> float -> float) -> t -> t -> t
  val for_all : (float -> bool) -> t -> bool
  val exists : (float -> bool) -> t -> bool
  val mem : float -> t -> bool
  val mem_ieee : float -> t -> bool
  val find_opt : (float -> bool) -> t -> float option
  val find_index : (float-> bool) -> t -> int option
  val find_map : (float -> 'a option) -> t -> 'a option
  val find_mapi : (int -> float -> 'a option) -> t -> 'a option
  val sort : (float -> float -> int) -> t -> unit
  val stable_sort : (float -> float -> int) -> t -> unit
  val fast_sort : (float -> float -> int) -> t -> unit
  val to_seq : t -> float Seq.t
  val to_seqi : t -> (int * float) Seq.t
  val of_seq : float Seq.t -> t
  val map_to_array : (float -> 'a) -> t -> 'a array
  val map_from_array : ('a -> float) -> 'a array -> t
  val unsafe_get : t -> int -> float
  val unsafe_set : t -> int -> float -> unit

  (* From Sys, rather than Float.Array *)
  val max_length : int
end

module Flat_float_array : S = struct
  include Stdlib.Float.Array
  let max_length = Sys.max_floatarray_length
end

(* module [Array] specialized to [float] and with a few changes,
   satisfies signature S *)
module Float_array : S = struct
  include Stdlib.Array
  let create = create_float
  let map_to_array f a = map f a
  let map_from_array f a = map f a
  let mem_ieee x a = exists ((=) x) a
  type t = float array
  let max_length = Sys.max_array_length
end

module Test_float_u_array : S = struct
  include Float_u_array

  module Float_u = Stdlib_upstream_compatible.Float_u

  let to_float = Float_u.to_float
  let of_float = Float_u.of_float

  type t = float# array

  let empty () = make 0 (of_float 0.0)
  let to_seq a =
    let rec aux i () =
      if i < length a
      then
        let x = unsafe_get a i in
        Seq.Cons (to_float x, aux (i+1))
      else Seq.Nil
    in
    aux 0

  let to_seqi a =
    let rec aux i () =
      if i < length a
      then
        let x = unsafe_get a i in
        Seq.Cons ((i,to_float x), aux (i+1))
      else Seq.Nil
    in
    aux 0

  let of_rev_list = function
      [] -> empty ()
    | hd::tl as l ->
      let len = List.length l in
      let a = make len (of_float hd) in
      let rec fill i = function
          [] -> a
        | hd::tl -> unsafe_set a i (of_float hd); fill (i-1) tl
      in
      fill (len-2) tl

  let of_seq i =
    let l = Seq.fold_left (fun acc x -> x::acc) [] i in
    of_rev_list l


  let create l = make l (of_float 0.0)
  let to_list t = fold_right (fun f l -> (to_float f)::l) t []

  let of_list l =
    let len = List.length l in
    let res = create len in
    List.iteri (fun idx f -> set res idx (of_float f)) l;
    res
  let max_length = Sys.max_floatarray_length
  let get t idx = to_float (get t idx)
  let set t idx v = set t idx (of_float v)

  let make l f = make l (of_float f)
  let init l f = init l (fun i -> of_float (f i))
  let fill a ofs len v = fill a ofs len (of_float v)
  let iter f t = iter (fun v -> f (to_float v)) t
  let iteri f t = iteri (fun i v -> f i (to_float v)) t
  let map f t = map (fun v -> of_float (f (to_float v))) t
  let map_inplace f t = map_inplace (fun v -> of_float (f (to_float v))) t
  let mapi f t = mapi (fun i v -> of_float (f i (to_float v))) t
  let mapi_inplace f t = mapi_inplace (fun i v -> of_float (f i (to_float v))) t
  let fold_left f acc t = fold_left (fun acc v -> f acc (to_float v)) acc t
  let fold_right f t acc = fold_right (fun v acc -> f (to_float v) acc) t acc

  let iter2 f a b = iter2 (fun v1 v2 -> f (to_float v1) (to_float v2)) a b
  let map2 f a b = map2 (fun v1 v2 -> of_float (f (to_float v1) (to_float v2))) a b
  let for_all f t = for_all (fun v -> f (to_float v)) t
  let exists f t = exists (fun v -> f (to_float v)) t
  let mem v t = mem (of_float v) t
  let mem_ieee goal t = exists (fun v -> v = goal) t

  let find_index f t = find_index (fun v -> f (to_float v)) t
  let find_opt f t =
    match find_index f t with
    | None -> None
    | Some idx -> Some (get t idx)
  let find_map f t = find_map (fun v -> f (to_float v)) t
  let find_mapi f t = find_mapi (fun i v -> f i (to_float v)) t

  let sort f t = sort (fun a b -> f (to_float a) (to_float b)) t
  let stable_sort f t = stable_sort (fun a b -> f (to_float a) (to_float b)) t
  let fast_sort f t = fast_sort (fun a b -> f (to_float a) (to_float b)) t

  let map_to_array f t =
    if length t = 0 then [||] else begin
      let res = Array.make (length t) (f (get t 0)) in
      iteri (fun idx v -> if idx > 0 then Array.set res idx (f v)) t;
      res
    end

  let map_from_array f a =
    if Array.length a = 0 then empty () else begin
      let res = make (Array.length a) (f (Array.get a 0)) in
      Array.iteri (fun idx v -> if idx > 0 then set res idx (f v)) a;
      res
    end

  let unsafe_get t idx = to_float (unsafe_get t idx)
  let unsafe_set t idx v = unsafe_set t idx (of_float v)

end


module Test (A : S) : sig end = struct

  (* auxiliary functions *)

  let neg_zero = 1.0 /. neg_infinity in

  let rec check_i_upto a i =
    if i >= 0 then begin
      assert (A.get a i = Float.of_int i);
      check_i_upto a (i - 1);
    end
  in

  let check_i a = check_i_upto a (A.length a - 1) in

  let check_inval f arg =
    match f arg with
    | _ -> assert false
    | exception (Invalid_argument _) -> ()
    | exception _ -> assert false
  in

  (* [make] [set] [get] *)
  let a = A.make 1000 1.0 in
  for i = 0 to 499 do A.set a i (Float.of_int i) done;
  let rec loop i =
    if i >= 0 then begin
      assert (A.get a i = (if i < 500 then Float.of_int i else 1.0));
      loop (i - 1);
    end
  in loop 999;
  check_inval (A.get a) (-1);
  check_inval (A.get a) (1000);
  check_inval (fun i -> A.set a i 1.0) (-1);
  check_inval (fun i -> A.set a i 1.0) 1000;
  check_inval A.create (-1);
  check_inval A.create (A.max_length + 1);
  check_inval (fun i -> A.make i 1.0) (-1);
  check_inval (fun i -> A.make i 1.0) (A.max_length + 1);

  let a = A.make 1001 1.0 in
  for i = 0 to 499 do A.set a i (Float.of_int i) done;
  let rec loop i =
    if i >= 0 then begin
      assert (A.get a i = (if i < 500 then Float.of_int i else 1.0));
      loop (i - 1);
    end
  in loop 1000;
  check_inval (A.get a) (-1);
  check_inval (A.get a) (1001);
  check_inval (fun i -> A.set a i 1.0) (-1);
  check_inval (fun i -> A.set a i 1.0) 1001;

  (* [length] *)
  let test_length l = assert (l = (A.length (A.create l))) in
  test_length 0;
  test_length 10;
  test_length 25;
  test_length 255;
  test_length 256;
  test_length 1000;
  test_length 123456;

  (* [init] *)
  let a = A.init 1000 Float.of_int in
  check_i a;
  let a = A.init 1001 Float.of_int in
  check_i a;
  check_inval (fun i -> A.init i Float.of_int) (-1);
  check_inval (fun i -> A.init i Float.of_int) (A.max_length + 1);

  (* [append] *)
  let check m n =
    let a = A.init m Float.of_int in
    let b = A.init n (fun x -> Float.of_int (x + m)) in
    let c = A.append a b in
    assert (A.length c = (m + n));
    check_i c;
  in
  check 0 0;
  check 0 100;
  check 1 100;
  check 100 0;
  check 100 1;
  check 100 100;
  check 1000 1000;
  check 1000 1001;
  check 1001 1000;
  check 1001 1001;
  (* check_inval omitted *)

  (* [concat] *)
  let check l =
    let f (len, acc) n =
      (len + n, A.init n (fun i -> Float.of_int (len + i)) :: acc)
    in
    let (total, ll) = List.fold_left f (0, []) l in
    let b = A.concat (List.rev ll) in
    assert (A.length b = total);
    check_i b;
  in
  check [0; 0; 0];
  check [1; 10; 100];
  check [10; 0];
  check [0];
  check [1000; 1000; 1000];
  check [];
  check [1001; 1000; 1000];
  check [1000; 1001; 1000];
  check [1000; 1000; 1001];
  check [1001; 1001; 1001];
  (* check_inval omitted *)

  (* [sub] *)
  let a = A.init 1000 (fun i -> Float.of_int (i - 100)) in
  let b = A.sub a 100 200 in
  check_i b;
  assert (A.length b = 200);
  let b = A.sub a 1000 0 in
  check_i b;
  assert  (A.length b = 0);
  check_inval (A.sub a (-1)) 0;
  check_inval (A.sub a 0) (-1);
  check_inval (A.sub a 0) 1001;
  check_inval (A.sub a 1000) 1;

  let a = A.init 1001 (fun i -> Float.of_int (i - 101)) in
  let b = A.sub a 101 199 in
  check_i b;
  assert (A.length b = 199);
  let b = A.sub a 1001 0 in
  check_i (A.sub a 1001 0);
  assert  (A.length b = 0);
  check_inval (A.sub a (-1)) 0;
  check_inval (A.sub a 0) (-1);
  check_inval (A.sub a 0) 1002;
  check_inval (A.sub a 1001) 1;

  (* [copy] *)
  let check len =
    let a = A.init len Float.of_int in
    let b = A.copy a in
    check_i b;
    assert (A.length b = len);
  in
  check 0;
  check 1;
  check 128;
  check 1023;

  (* [blit] [fill] *)
  let test_blit_fill data initval ofs len =
    let a = A.of_list data in
    let b = A.create (List.length data) in
    A.blit a 0 b 0 (A.length b);
    assert (a = b);
    A.fill b ofs len initval;
    let rec check i = function
      | [] -> ()
      | hd :: tl ->
          assert (A.get b i = (if i >= ofs && i < ofs + len
                               then initval else hd));
          check (i + 1) tl;
    in
    check 0 data
  in
  test_blit_fill [1.0;2.0;5.0;8.123;-100.456;212e19] 3.1415 3 2;
  let a = A.create 100 in
  check_inval (A.fill a (-1) 0) 1.0;
  check_inval (A.fill a 0 (-1)) 1.0;
  check_inval (A.fill a 0 101) 1.0;
  check_inval (A.fill a 100 1) 1.0;
  check_inval (A.fill a 101 0) 1.0;
  check_inval (A.blit a (-1) a 0) 0;
  check_inval (A.blit a 0 a 0) (-1);
  check_inval (A.blit a 0 a 0) 101;
  check_inval (A.blit a 100 a 0) 1;
  check_inval (A.blit a 101 a 0) 0;
  check_inval (A.blit a 0 a (-1)) 0;
  check_inval (A.blit a 0 a 100) 1;
  check_inval (A.blit a 0 a 101) 0;
  let a = A.create 101 in
  check_inval (A.fill a (-1) 0) 1.0;
  check_inval (A.fill a 0 (-1)) 1.0;
  check_inval (A.fill a 0 102) 1.0;
  check_inval (A.fill a 101 1) 1.0;
  check_inval (A.fill a 102 0) 1.0;
  check_inval (A.blit a (-1) a 0) 0;
  check_inval (A.blit a 0 a 0) (-1);
  check_inval (A.blit a 0 a 0) 102;
  check_inval (A.blit a 101 a 0) 1;
  check_inval (A.blit a 102 a 0) 0;
  check_inval (A.blit a 0 a (-1)) 0;
  check_inval (A.blit a 0 a 101) 1;
  check_inval (A.blit a 0 a 102) 0;
  let test_blit_overlap a ofs1 ofs2 len =
    let a = A.of_list a in
    let b = A.copy a in
    A.blit a ofs1 a ofs2 len;
    for i = 0 to len - 1 do
      assert (A.get b (ofs1 + i) = A.get a (ofs2 + i))
    done
  in
  test_blit_overlap [1.; 2.; 3.; 4.] 1 2 2;

  (* [to_list] [of_list] *)
  let a = A.init 1000 Float.of_int in
  assert (compare a (A.of_list (A.to_list a)) = 0);
  let a = A.init 1001 Float.of_int in
  assert (compare a (A.of_list (A.to_list a)) = 0);
  let a = A.init 0 Float.of_int in
  assert (compare a (A.of_list (A.to_list a)) = 0);
  (* check_inval omitted *)

  (* [iter] *)
  let a = A.init 300 (Float.of_int) in
  let r = ref 0.0 in
  A.iter (fun x -> assert (x = !r); r := x +. 1.0) a;
  A.iter (fun _ -> assert false) (A.create 0);
  assert (!r = 300.0);

  let a = A.init 301 (Float.of_int) in
  let r = ref 0.0 in
  A.iter (fun x -> assert (x = !r); r := x +. 1.0) a;
  assert (!r = 301.0);

  (* [iteri] *)
  let a = A.init 300 Float.of_int in
  let r = ref 0 in
  let f i x =
    assert (i = !r);
    assert (x = Float.of_int i);
    r := i + 1
  in
  A.iteri f a;
  A.iteri (fun _ _ -> assert false) (A.create 0);
  assert (!r = 300);

  let a = A.init 301 Float.of_int in
  let r = ref 0 in
  let f i x =
    assert (i = !r);
    assert (x = Float.of_int i);
    r := i + 1
  in
  A.iteri f a;
  A.iteri (fun _ _ -> assert false) (A.create 0);
  assert (!r = 301);

  (* [map], test result and order of evaluation *)
  let a = A.init 500 Float.of_int in
  let r = ref 0.0 in
  let f x =
    assert (x = !r);
    r := !r +. 1.0;
    x -. 1.0
  in
  let b = A.map f a in
  check_i (A.sub b 1 499);

  let a = A.init 501 Float.of_int in
  let r = ref 0.0 in
  let f x =
    assert (x = !r);
    r := !r +. 1.0;
    x -. 1.0
  in
  let b = A.map f a in
  check_i (A.sub b 1 500);

  (* [mapi], test result and order of evaluation *)
  let a = A.init 500 Float.of_int in
  let r = ref 0.0 in
  let f i x =
    assert (x = Float.of_int i);
    assert (x = !r);
    r := !r +. 1.0;
    x -. 1.0
  in
  let b = A.mapi f a in
  check_i (A.sub b 1 499);

  let a = A.init 501 Float.of_int in
  let r = ref 0.0 in
  let f i x =
    assert (x = Float.of_int i);
    assert (x = !r);
    r := !r +. 1.0;
    x -. 1.0
  in
  let b = A.mapi f a in
  check_i (A.sub b 1 500);

  (* [fold_left], test result and order of evaluation *)
  let a = A.init 500 Float.of_int in
  let f acc x =
    assert (acc = x);
    x +. 1.0
  in
  let acc = A.fold_left f 0.0 a in
  assert (acc = 500.0);

  let a = A.init 501 Float.of_int in
  let acc = A.fold_left f 0.0 a in
  assert (acc = 501.0);

  (* [fold_right], test result and order of evaluation *)
  let a = A.init 500 Float.of_int in
  let f x acc =
    assert (x = acc -. 1.0);
    x
  in
  let acc = A.fold_right f a 500.0 in
  assert (acc = 0.0);

  let a = A.init 501 Float.of_int in
  let acc = A.fold_right f a 501.0 in
  assert (acc = 0.0);

  (* [iter2], test result and order of evaluation *)
  let a = A.init 123 Float.of_int in
  let b = A.init 123 Float.of_int in
  let r = ref 0.0 in
  let f x y =
    assert (x = !r);
    assert (y = !r);
    r := !r +. 1.0;
  in
  A.iter2 f a b;
  let c = A.create 456 in
  check_inval (A.iter2 (fun _ _ -> assert false) a) c;
  check_inval (A.iter2 (fun _ _ -> assert false) c) a;

  let a = A.init 124 Float.of_int in
  let b = A.init 124 Float.of_int in
  let r = ref 0.0 in
  let f x y =
    assert (x = !r);
    assert (y = !r);
    r := !r +. 1.0;
  in
  A.iter2 f a b;

  (* [map2], test result and order of evaluation *)
  let a = A.init 456 Float.of_int in
  let b = A.init 456 (fun i -> Float.of_int i /. 2.0) in
  let r = ref 0.0 in
  let f x y =
    assert (x = !r);
    assert (y = !r /. 2.0);
    r := !r +. 1.0;
    2.0 *. (x -. y)
  in
  let c = A.map2 f a b in
  check_i c;
  let d = A.create 455 in
  check_inval (A.map2 (fun _ _ -> assert false) a) d;
  check_inval (A.map2 (fun _ _ -> assert false) d) a;

  let a = A.init 457 Float.of_int in
  let b = A.init 457 (fun i -> Float.of_int i /. 2.0) in
  let r = ref 0.0 in
  let f x y =
    assert (x = !r);
    assert (y = !r /. 2.0);
    r := !r +. 1.0;
    2.0 *. (x -. y)
  in
  let c = A.map2 f a b in
  check_i c;

  (* [for_all], test result and order of evaluation *)
  let a = A.init 777 Float.of_int in
  let r = ref 0.0 in
  let f x =
    assert (x = !r);
    r := x +. 1.0;
    true
  in
  assert (A.for_all f a);
  let f x = assert (x = 0.0); false in
  assert (not (A.for_all f a));

  let a = A.init 778 Float.of_int in
  let r = ref 0.0 in
  let f x =
    assert (x = !r);
    r := x +. 1.0;
    true
  in
  assert (A.for_all f a);
  let f x = assert (x = 0.0); false in
  assert (not (A.for_all f a));

  (* [exists], test result and order of evaluation *)
  let a = A.init 777 Float.of_int in
  let r = ref 0.0 in
  let f x =
    assert (x = !r);
    r := x +. 1.0;
    false
  in
  assert (not (A.exists f a));
  let f x = assert (x = 0.0); true in
  assert (A.exists f a);

  let a = A.init 778 Float.of_int in
  let r = ref 0.0 in
  let f x =
    assert (x = !r);
    r := x +. 1.0;
    false
  in
  assert (not (A.exists f a));
  let f x = assert (x = 0.0); true in
  assert (A.exists f a);

  (* [mem] *)
  let a = A.init 7777 Float.of_int in
  assert (A.mem 0.0 a);
  assert (A.mem 7776.0 a);
  assert (not (A.mem (-1.0) a));
  assert (not (A.mem 7777.0 a));
  let check v =
    A.set a 1000 v;
    assert (A.mem v a);
  in
  List.iter check [infinity; neg_infinity; neg_zero; nan];

  let a = A.init 7778 Float.of_int in
  assert (A.mem 0.0 a);
  assert (A.mem 7777.0 a);
  assert (not (A.mem (-1.0) a));
  assert (not (A.mem 7778.0 a));
  let check v =
    A.set a 1001 v;
    assert (A.mem v a);
  in
  List.iter check [infinity; neg_infinity; neg_zero; nan];

  (* [mem_ieee] *)
  let a = A.init 7777 Float.of_int in
  assert (A.mem_ieee 0.0 a);
  assert (A.mem_ieee 7776.0 a);
  assert (not (A.mem_ieee (-1.0) a));
  assert (not (A.mem_ieee 7777.0 a));
  let check v =
    A.set a 1000 v;
    assert (A.mem_ieee v a);
  in
  List.iter check [infinity; neg_infinity; neg_zero];
  A.set a 0 nan;
  assert (not (A.mem_ieee nan a));

  (* [find_opt], test result and order of evaluation *)
  let a = A.init 777 Float.of_int in
  let r = ref 0.0 in
  let f x =
    assert (x = !r);
    r := x +. 1.0;
    false
  in
  assert (Option.is_none (A.find_opt f a));
  let f x = assert (x = 0.0); true in
  assert (Option.is_some (A.find_opt f a));

  (* [find_index], test result and order of evaluation *)
  let a = A.init 777 Float.of_int in
  let r = ref 0.0 in
  let f x =
    assert (x = !r);
    r := x +. 1.0;
    false
  in
  assert (Option.is_none (A.find_index f a));
  let f x = assert (x = 0.0); true in
  assert (Option.get (A.find_index f a) = 0);

  (* [find_map], test result and order of evaluation *)
  let a = A.init 777 Float.of_int in
  let r = ref 0.0 in
  let f x =
    assert (x = !r);
    r := x +. 1.0;
    None
  in
  assert (Option.is_none (A.find_map f a));
  let f x = assert (x = 0.0); Some "abc" in
  assert (Option.get (A.find_map f a) = "abc");

  (* [find_mapi], test result and order of evaluation *)
  let a = A.init 777 Float.of_int in
  let r = ref 0.0 in
  let r_i = ref 0 in
  let f i x =
    assert (i = !r_i);
    assert (x = !r);
    r_i := !r_i + 1;
    r := x +. 1.0;
    None
  in
  assert (Option.is_none (A.find_mapi f a));
  let f i x =
    assert (i = 0);
    assert (x = 0.0);
    Some "abc"
  in
  assert (Option.get (A.find_mapi f a) = "abc");

  (* [sort] [fast_sort] [stable_sort] *)
  let check_sort sort cmp a =
    let rec check_sorted a i =
      if i + 1 < A.length a then begin
        assert (cmp (A.get a i) (A.get a (i + 1)) <= 0);
        check_sorted a (i + 1);
      end
    in
    let rec check_permutation a b i =
      let p = Array.make (A.length a) true in
      let rec find lo hi x =
        assert (lo < hi);
        if hi = lo + 1 then begin
          assert (cmp (A.get a lo) x = 0);
          assert (p.(lo));
          p.(lo) <- false;
        end else begin
          let mid = (lo + hi) / 2 in
          assert (lo < mid && mid < hi);
          match cmp (A.get a (mid - 1)) x with
          | 0 when p.(mid - 1) -> find lo mid x
          | 0 -> find mid hi x
          | c when c < 0 -> find mid hi x
          | c when c > 0 -> find lo mid x
          | _ -> assert false
        end
      in
      A.iter (find 0 (A.length a)) b
    in
    let b = A.copy a in
    sort cmp a;
    check_sorted a 0;
    check_permutation a b 0;
  in
  Random.init 123;
  let rand_float _ =
    match Random.int 1004 with
    | 1000 -> nan
    | 1001 -> infinity
    | 1002 -> neg_infinity
    | 1003 -> neg_zero
    | n when n < 500 -> Random.float 1.0
    | _ -> -. Random.float 1.0
  in
  let check s =
    let a = A.init 5 Float.of_int in
    check_sort s Stdlib.compare a; (* already sorted *)
    check_sort s (fun x y -> Stdlib.compare y x) a; (* reverse-sorted *)

    let a = A.init 6 Float.of_int in
    check_sort s Stdlib.compare a; (* already sorted *)
    check_sort s (fun x y -> Stdlib.compare y x) a; (* reverse-sorted *)

    let a = A.of_list [nan; neg_infinity; neg_zero; 0.; infinity] in
    check_sort s Stdlib.compare a; (* already sorted *)
    check_sort s (fun x y -> Stdlib.compare y x) a; (* reverse-sorted *)

    let a = A.init 50000 rand_float in
    check_sort s Stdlib.compare a;
    let a = A.init 50001 rand_float in
    check_sort s Stdlib.compare a;
    let a = A.make 1000 1.0 in
    check_sort s Stdlib.compare a;
    let a = A.make 1001 1.0 in
    check_sort s Stdlib.compare a;
    let a = A.append (A.make 1000 1.0) (A.make 1000 2.0) in
    check_sort s Stdlib.compare a;
    let a = A.append (A.make 1001 1.0) (A.make 1001 2.0) in
    check_sort s Stdlib.compare a;
  in
  check A.sort;
  check A.stable_sort;
  check A.fast_sort;

  (* [to_seq] *)
  let check_seq a =
    let r = ref 0 in
    let f x =
      assert (A.get a !r = x);
      r := !r + 1;
    in
    let s = A.to_seq a in
    Seq.iter f s;
  in
  check_seq (A.init 999 Float.of_int);
  check_seq (A.init 1000 Float.of_int);
  check_seq (A.create 0);

  (* [to_seqi] *)
  let check_seqi a =
    let r = ref 0 in
    let f (i, x) =
      assert (i = !r);
      assert (A.get a !r = x);
      r := !r + 1;
    in
    let s = A.to_seqi a in
    Seq.iter f s;
  in
  check_seqi (A.init 999 Float.of_int);
  check_seqi (A.init 1000 Float.of_int);
  check_seqi (A.create 0);

  (* [of_seq] *)
  let r = ref 0 in
  let rec f () =
    if !r = 100 then Seq.Nil else begin
      let res = Seq.Cons (Float.of_int !r, f) in
      r := !r + 1;
      res
    end
  in
  let a = A.of_seq f in
  assert (a = A.init 100 Float.of_int);
  assert (A.of_seq Seq.empty = A.create 0);

  (* [map_to_array] *)
  let r = ref 0 in
  let f x =
    assert (x = Float.of_int !r);
    r := !r + 1;
    x *. 2.0
  in
  let a = A.init 876 Float.of_int in
  let ar1 = A.map_to_array f a in
  let ar2 = Array.init 876 (fun x -> Float.of_int (2 * x)) in
  assert (ar1 = ar2);
  let ar = A.map_to_array (fun _ -> assert false) (A.create 0) in
  assert (ar = [| |]);

  (* [map_from_array] *)
  let r = ref 0 in
  let f x =
    assert (x = Float.of_int !r);
    r := !r + 1;
    x *. 2.0
  in
  let ar = Array.init 876 Float.of_int in
  let a1 = A.map_from_array f ar in
  let a2 = A.init 876 (fun x -> Float.of_int (2 * x)) in
  assert (a1 = a2);
  let a = A.map_from_array (fun _ -> assert false) [| |] in
  assert (a = A.create 0);

  (* comparisons *)
  let normalize_comparison n =
    if n = 0 then 0 else if n < 0 then -1 else 1
  in
  let check c l1 l2 =
    assert (c = (normalize_comparison (compare (A.of_list l1) (A.of_list l2))))
  in
  check 0    [0.0; 0.25; -4.0; 3.141592654; nan]
             [0.0; 0.25; -4.0; 3.141592654; nan];
  check (-1) [0.0; 0.25; nan]
             [0.0; 0.25; 3.14];
  check (-1) [0.0; 0.25; -4.0]
             [0.0; 0.25; 3.14159];
  check 1    [0.0; 2.718; -4.0]
             [0.0; 0.25; 3.14159];
  check 1    [0.0; 2.718; -4.0]
             [nan; 0.25; 3.14159];

  (* [unsafe_get] [unsafe_set] *)
  let a = A.create 3 in
  for i = 0 to 2 do A.unsafe_set a i (float i) done;
  for i = 0 to 2 do assert (A.unsafe_get a i = float i) done;

  let a = A.create 4 in
  for i = 0 to 3 do A.unsafe_set a i (float i) done;
  for i = 0 to 3 do assert (A.unsafe_get a i = float i) done;

  (* I/O *)
  let test_structured_io value =
    let (tmp, oc) =
      Filename.open_temp_file ~mode:[Open_binary] "floatarray" ".data"
    in
    Marshal.to_channel oc value [];
    close_out oc;
    let ic = open_in_bin tmp in
    let value' = Marshal.from_channel ic in
    close_in ic;
    Sys.remove tmp;
    assert (compare value value' = 0)
  in
  let l = [0.; 0.25; -4.; 3.14159265; nan; infinity; neg_infinity; neg_zero] in
  test_structured_io (A.of_list l);

  (* map_inplace *)
  let a = A.init 4 (fun i -> Float.of_int (i + 1)) in
  A.map_inplace (fun x -> 2. *. x) a;
  let got = A.map_to_array Fun.id a in
  let expected = [|2.; 4.; 6.; 8.|] in
  assert (Array.for_all2 Float.equal got expected);

  (* mapi_inplace *)
  let a = A.init 4 (fun i -> Float.of_int (i + 1)) in
  A.mapi_inplace (fun i x -> 1. +. (Float.of_int i) +. x) a;
  let got = A.map_to_array Fun.id a in
  let expected = [|2.; 4.; 6.; 8.|] in
  assert (Array.for_all2 Float.equal got expected)
end

module T3 = Test (Test_float_u_array)

(* Extra tests for functions not covered above *)
module Float_u = Stdlib_upstream_compatible.Float_u
let () =
  let open Float_u_array in
  let check_inval f arg =
    match f arg with
    | _ -> assert false
    | exception (Invalid_argument _) -> ()
    | exception _ -> assert false
  in

  (* make_matrix *)
  check_inval (make_matrix (-1) 1) (Float_u.of_int 1);
  check_inval (make_matrix 1 (-1)) (Float_u.of_int 1);
  let check_matrix a =
    let row_len = Array.length a in
    assert (row_len > 0);
    let col_len = length (a.(0)) in
    for row = 0 to (row_len - 1) do
      assert (length (a.(row)) = col_len);
      for col = 0 to (col_len - 1) do
        assert Float_u.(equal (get (a.(row)) col) (of_int 1))
      done
    done in
  let a = make_matrix 100 100 (Float_u.of_int 1) in
  check_matrix a;
  let a = make_matrix 101 100 (Float_u.of_int 1) in
  check_matrix a;
  let a = make_matrix 101 101 (Float_u.of_int 1) in
  check_matrix a;
  let a = make_matrix 100 101 (Float_u.of_int 1) in
  check_matrix a;

  (* for_all2 *)
  let test a =
    let r = ref 0.0 in
    let f x y =
      let x = Float_u.to_float x in
      let y = Float_u.to_float y in
      assert (x = !r);
      assert (y = !r);
      r := x +. 1.0;
      true
    in
    assert (for_all2 f a a);
    let f x y =
      let x = Float_u.to_float x in
      let y = Float_u.to_float y in
      assert (x = 0.0); assert (y = 0.0); false in
    if length a > 0 then assert (not (for_all2 f a a))

  in
  let a = init 777 Float_u.of_int in
  test a;
  let a = init 778 Float_u.of_int in
  test a;
  let a = init 0 Float_u.of_int in
  test a;
  check_inval (fun x -> for_all2 (fun _ _ -> true) (make 100 x) (make 101 x))
    (Float_u.of_int 1);

  (* exists2 *)
  let test a =
    let r = ref 0.0 in
    let f x y =
      let x = Float_u.to_float x in
      let y = Float_u.to_float y in
      assert (x = !r);
      assert (y = !r);
      r := x +. 1.0;
      false
    in
    assert (not (exists2 f a a));
    let f x y =
      let x = Float_u.to_float x in
      let y = Float_u.to_float y in
      assert (x = 0.0); assert (y = 0.0); true in
    if length a > 0 then assert (exists2 f a a)

  in
  let a = init 777 Float_u.of_int in
  test a;
  let a = init 778 Float_u.of_int in
  test a;
  let a = init 0 Float_u.of_int in
  test a;
  check_inval (fun x -> exists2 (fun _ _ -> true) (make 100 x) (make 101 x))
    (Float_u.of_int 1)

module Test_same_memory_layout_as_floatarray = struct
  (* It is currently the case that [float# array] has the same memory
     representation as [floatarray]. This test is here to document
     that fact and help catch issues when this assumption is no longer
     true. *)

  external get_f : floatarray -> int -> float = "%floatarray_safe_get"
  let get_f (arr : float# array) idx =
    get_f (Obj.magic arr : floatarray) idx |> Float_u.of_float

  external set_f : floatarray -> int -> float -> unit = "%floatarray_safe_set"
  let set_f (arr : float# array) idx v =
    set_f (Obj.magic arr : floatarray) idx (Float_u.to_float v)

  let float_u_eq x y = Float_u.compare x y = 0
  let check_eq arr g =
    let open Float_u_array in
    for i = 0 to length arr - 1 do
      assert (float_u_eq (g arr i) (get arr i))
    done

  let () =
    let open Float_u_array in

    check_eq (Float_u_array.make 10 #1.) get_f;
    check_eq [| #1.; #2.; #3.|] get_f;

    let fill arr v =
      for i = 0 to length arr - 1 do
        set_f arr i v; assert(float_u_eq (get arr i) v)
      done
    in
    let check_all_eq arr v = assert (for_all (fun x -> float_u_eq x v) arr) in
    let arr = [| #1.; #2.; #3.|] in
    fill arr #0.; check_all_eq arr #0.;
    let arr = Float_u_array.make 10 #1. in
    fill arr #0.; check_all_eq arr #0.;
    ()
end
