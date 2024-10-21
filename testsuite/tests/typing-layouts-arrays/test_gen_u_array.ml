(* Provides functor [Make_boxed] that constructs a module with a
   simliar interface as stdlib [Array] specialized to int64, int32, or
   nativeint. This module can then be passed to functor [Test] to check
   for correctness. *)

module type Element_intf = sig
  type t
  val of_int : int -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val neg : t -> t
  val max_val : t
  val min_val : t
  val rand : t -> t
  val compare : t -> t -> int
  val print : t -> unit
end

module type S = sig
  type t
  type element_t
  val length : t -> int
  val get : t -> int -> element_t
  val set : t -> int -> element_t -> unit
  val make : int -> element_t -> t
  val init : int -> (int -> element_t) -> t
  val make_matrix : int -> int -> element_t -> t array
  val append : t -> t -> t
  val concat : t list -> t
  val sub : t -> int -> int -> t
  val copy : t -> t
  val fill : t -> int -> int -> element_t -> unit
  val blit : t -> int -> t -> int -> int -> unit
  val to_list : t -> element_t list
  val of_list : element_t list -> t
  val iter : (element_t -> unit) -> t -> unit
  val iteri : (int -> element_t -> unit) -> t -> unit
  val map : (element_t -> element_t) -> t -> t
  val map_inplace : (element_t -> element_t) -> t -> unit
  val mapi : (int -> element_t -> element_t) -> t -> t
  val mapi_inplace : (int -> element_t -> element_t) -> t -> unit
  val fold_left : ('a -> element_t -> 'a) -> 'a -> t -> 'a
  val fold_right : (element_t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter2 : (element_t -> element_t -> unit) -> t -> t -> unit
  val map2 : (element_t -> element_t -> element_t) -> t -> t -> t
  val for_all : (element_t -> bool) -> t -> bool
  val for_all2 : (element_t -> element_t -> bool) -> t -> t -> bool
  val exists : (element_t -> bool) -> t -> bool
  val exists2 : (element_t -> element_t -> bool) -> t -> t -> bool
  val mem : element_t -> t -> bool
  val find_opt : (element_t -> bool) -> t -> element_t option
  val find_index : (element_t-> bool) -> t -> int option
  val find_map : (element_t -> 'a option) -> t -> 'a option
  val find_mapi : (int -> element_t -> 'a option) -> t -> 'a option
  val sort : (element_t -> element_t -> int) -> t -> unit
  val stable_sort : (element_t -> element_t -> int) -> t -> unit
  val fast_sort : (element_t -> element_t -> int) -> t -> unit
  val to_seq : t -> element_t Seq.t
  val to_seqi : t -> (int * element_t) Seq.t
  val of_seq : element_t Seq.t -> t
  val map_to_array : (element_t -> 'a) -> t -> 'a array
  val map_from_array : ('a -> element_t) -> 'a array -> t
  val unsafe_get : t -> int -> element_t
  val unsafe_set : t -> int -> element_t -> unit
  val equal : t -> t -> bool
  (* From Sys, rather than Float.Array *)
  val max_length : int

  module I : Element_intf with type t = element_t
end

module Make_boxed (Arg : sig
  module M : Gen_u_array.S
  module I : Element_intf
  module E : sig
    val to_boxed : M.element_arg -> I.t
    val of_boxed : I.t -> M.element_arg
  end
end) : S with type t = Arg.M.t
         and type element_t = Arg.I.t = struct
  include Arg.M
  include Arg.E

  module I = Arg.I

  type element_t = I.t

  let empty () = make 0 (of_boxed (I.of_int 0))
  let to_seq a =
    let rec aux i () =
      if i < length a
      then
        let x = unsafe_get a i in
        Seq.Cons (to_boxed x, aux (i+1))
      else Seq.Nil
    in
    aux 0

  let to_seqi a =
    let rec aux i () =
      if i < length a
      then
        let x = unsafe_get a i in
        Seq.Cons ((i,to_boxed x), aux (i+1))
      else Seq.Nil
    in
    aux 0

  let of_rev_list = function
      [] -> empty ()
    | hd::tl as l ->
      let len = List.length l in
      let a = make len (of_boxed hd) in
      let rec fill i = function
          [] -> a
        | hd::tl -> unsafe_set a i (of_boxed hd); fill (i-1) tl
      in
      fill (len-2) tl

  let of_seq i =
    let l = Seq.fold_left (fun acc x -> x::acc) [] i in
    of_rev_list l


  let to_list t = fold_right (fun f l -> (to_boxed f)::l) t []

  let of_list l =
    let len = List.length l in
    let res = make len (of_boxed (I.of_int 0)) in
    List.iteri (fun idx f -> set res idx (of_boxed f)) l;
    res

  let get t idx = to_boxed (get t idx)
  let set t idx v = set t idx (of_boxed v)

  let make l f = make l (of_boxed f)
  let init l f = init l (fun i -> of_boxed (f i))
  let make_matrix sx sy init = make_matrix sx sy (of_boxed init)
  let fill a ofs len v = fill a ofs len (of_boxed v)
  let iter f t = iter (fun v -> f (to_boxed v)) t
  let iteri f t = iteri (fun i v -> f i (to_boxed v)) t
  let map f t = map (fun v -> of_boxed (f (to_boxed v))) t
  let map_inplace f t = map_inplace (fun v -> of_boxed (f (to_boxed v))) t
  let mapi f t = mapi (fun i v -> of_boxed (f i (to_boxed v))) t
  let mapi_inplace f t = mapi_inplace (fun i v -> of_boxed (f i (to_boxed v))) t
  let fold_left f acc t = fold_left (fun acc v -> f acc (to_boxed v)) acc t
  let fold_right f t acc = fold_right (fun v acc -> f (to_boxed v) acc) t acc

  let iter2 f a b = iter2 (fun v1 v2 -> f (to_boxed v1) (to_boxed v2)) a b
  let map2 f a b = map2 (fun v1 v2 -> of_boxed (f (to_boxed v1) (to_boxed v2))) a b
  let for_all f t = for_all (fun v -> f (to_boxed v)) t
  let exists f t = exists (fun v -> f (to_boxed v)) t
  let mem v t = mem (of_boxed v) t

  let find_index f t = find_index (fun v -> f (to_boxed v)) t
  let find_opt f t =
    match find_index f t with
    | None -> None
    | Some idx -> Some (get t idx)
  let find_map f t = find_map (fun v -> f (to_boxed v)) t
  let find_mapi f t = find_mapi (fun i v -> f i (to_boxed v)) t

  let sort f t = sort (fun a b -> f (to_boxed a) (to_boxed b)) t
  let stable_sort f t = stable_sort (fun a b -> f (to_boxed a) (to_boxed b)) t
  let fast_sort f t = fast_sort (fun a b -> f (to_boxed a) (to_boxed b)) t

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

  let unsafe_get t idx = to_boxed (unsafe_get t idx)
  let unsafe_set t idx v = unsafe_set t idx (of_boxed v)
  let equal = for_all2 (fun x y -> I.compare (to_boxed x) (to_boxed y) = 0)
  let for_all2 f t1 t2 = for_all2 (fun a b -> f (to_boxed a) (to_boxed b)) t1 t2
  let exists2 f t1 t2 = exists2 (fun a b -> f (to_boxed a) (to_boxed b)) t1 t2
end

module Test (A : S) : sig end = struct
  let module I = A.I in
  let assert_eq x y = assert (I.compare x y = 0) in

  (* auxiliary functions *)
  let rec check_i_upto a i =
    if i >= 0 then begin
      assert_eq (A.get a i) (I.of_int i);
      check_i_upto a (i - 1);
    end
  in

  let check_i a = check_i_upto a (A.length a - 1) in

  let check_inval f arg =
    match f arg with
    | _ -> Format.printf "check_inval failed"; assert false
    | exception (Invalid_argument _) -> ()
    | exception _ -> assert false
  in

  (* [make] [set] [get] *)
  let a = A.make 1000 (I.of_int 1) in
  for i = 0 to 499 do A.set a i (I.of_int i) done;
  let rec loop i =
    if i >= 0 then begin
      assert_eq (A.get a i) (if i < 500 then I.of_int i else (I.of_int 1));
      loop (i - 1);
    end
  in loop 999;
  check_inval (A.get a) (-1);
  check_inval (A.get a) (1000);
  check_inval (fun i -> A.set a i (I.of_int 1)) (-1);
  check_inval (fun i -> A.set a i (I.of_int 1)) 1000;
  check_inval (fun i -> A.make i (I.of_int 1)) (-1);
  check_inval (fun i -> A.make i (I.of_int 1)) (A.max_length + 1);

  let a = A.make 1001 (I.of_int 1) in
  for i = 0 to 499 do A.set a i (I.of_int i) done;
  let rec loop i =
    if i >= 0 then begin
      assert_eq (A.get a i) (if i < 500 then I.of_int i else (I.of_int 1));
      loop (i - 1);
    end
  in loop 1000;
  check_inval (A.get a) (-1);
  check_inval (A.get a) (1001);
  check_inval (fun i -> A.set a i (I.of_int 1)) (-1);
  check_inval (fun i -> A.set a i (I.of_int 1)) 1001;

  (* [length] *)
  let test_length l = assert (l = (A.length (A.make l (I.of_int 1)))) in
  test_length 0;
  test_length 1;
  test_length 10;
  test_length 25;
  test_length 255;
  test_length 256;
  test_length 1000;
  test_length 1001;
  test_length 123456;

  (* [init] *)
  let a = A.init 1000 I.of_int in
  check_i a;
  let a = A.init 1001 I.of_int in
  check_i a;
  check_inval (fun i -> A.init i I.of_int) (-1);
  check_inval (fun i -> A.init i I.of_int) (A.max_length + 1);

  (* [append] *)
  let check m n =
    let a = A.init m I.of_int in
    let b = A.init n (fun x -> I.of_int (x + m)) in
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
      (len + n, A.init n (fun i -> I.of_int (len + i)) :: acc)
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
  let a = A.init 1000 (fun i -> I.of_int (i - 100)) in
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

  let a = A.init 1001 (fun i -> I.of_int (i - 101)) in
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
    let a = A.init len I.of_int in
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
    let b = A.make (List.length data) (I.of_int 1) in
    A.blit a 0 b 0 (A.length b);
    assert (A.equal a b);
    A.fill b ofs len initval;
    let rec check i = function
      | [] -> ()
      | hd :: tl ->
          assert_eq (A.get b i) (if i >= ofs && i < ofs + len
                                 then initval else hd);
          check (i + 1) tl;
    in
    check 0 data
  in
  test_blit_fill [I.of_int 1;I.of_int 2;I.of_int 5;I.of_int 8;I.of_int (-100);I.of_int 2120000000] (I.of_int 3) 3 2;
  let a = A.make 100 (I.of_int 0) in
  check_inval (A.fill a (-1) 0) (I.of_int 1);
  check_inval (A.fill a 0 (-1)) (I.of_int 1);
  check_inval (A.fill a 0 101) (I.of_int 1);
  check_inval (A.fill a 100 1) (I.of_int 1);
  check_inval (A.fill a 101 0) (I.of_int 1);
  check_inval (A.blit a (-1) a 0) 0;
  check_inval (A.blit a 0 a 0) (-1);
  check_inval (A.blit a 0 a 0) 101;
  check_inval (A.blit a 100 a 0) 1;
  check_inval (A.blit a 101 a 0) 0;
  check_inval (A.blit a 0 a (-1)) 0;
  check_inval (A.blit a 0 a 100) 1;
  check_inval (A.blit a 0 a 101) 0;
  let a = A.make 101 (I.of_int 0) in
  check_inval (A.fill a (-1) 0) (I.of_int 1);
  check_inval (A.fill a 0 (-1)) (I.of_int 1);
  check_inval (A.fill a 0 102) (I.of_int 1);
  check_inval (A.fill a 101 1) (I.of_int 1);
  check_inval (A.fill a 102 0) (I.of_int 1);
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
      assert_eq (A.get b (ofs1 + i)) (A.get a (ofs2 + i))
    done
  in
  test_blit_overlap [(I.of_int 1); (I.of_int 2); (I.of_int 3); (I.of_int 4)] 1 2 2;

  (* [to_list] [of_list] *)
  let a = A.init 1000 I.of_int in
  assert (A.equal a (A.of_list (A.to_list a)));
  let a = A.init 1001 I.of_int in
  assert (A.equal a (A.of_list (A.to_list a)));
  let a = A.init 0 I.of_int in
  assert (A.equal a (A.of_list (A.to_list a)));
  (* check_inval omitted *)

  (* [iter] *)
  let a = A.init 300 (I.of_int) in
  let r = ref (I.of_int 0) in
  A.iter (fun x -> assert_eq x !r; r := I.add x (I.of_int 1)) a;
  A.iter (fun _ -> assert false) (A.make 0 (I.of_int 0));
  assert_eq !r (I.of_int 300);

  let a = A.init 301 (I.of_int) in
  let r = ref (I.of_int 0) in
  A.iter (fun x -> assert_eq x !r; r := I.add x (I.of_int 1)) a;
  assert_eq !r (I.of_int 301);

  (* [iteri] *)
  let a = A.init 300 I.of_int in
  let r = ref 0 in
  let f i x =
    assert (i = !r);
    assert_eq x (I.of_int i);
    r := i + 1
  in
  A.iteri f a;
  A.iteri (fun _ _ -> assert false) (A.make 0 (I.of_int 0));
  assert (!r = 300);

  let a = A.init 301 I.of_int in
  let r = ref 0 in
  let f i x =
    assert (i = !r);
    assert_eq x (I.of_int i);
    r := i + 1
  in
  A.iteri f a;
  A.iteri (fun _ _ -> assert false) (A.make 0 (I.of_int 0));
  assert (!r = 301);

  (* [map], test result and order of evaluation *)
  let a = A.init 500 I.of_int in
  let r = ref (I.of_int 0) in
  let f x =
    assert_eq x (!r);
    r := I.add !r (I.of_int 1);
    I.sub x (I.of_int 1)
  in
  let b = A.map f a in
  check_i (A.sub b 1 499);

  let a = A.init 501 I.of_int in
  let r = ref (I.of_int 0) in
  let f x =
    assert_eq x (!r);
    r := I.add !r (I.of_int 1);
    I.sub x (I.of_int 1)
  in
  let b = A.map f a in
  check_i (A.sub b 1 500);

  (* [mapi], test result and order of evaluation *)
  let a = A.init 500 I.of_int in
  let r = ref (I.of_int 0) in
  let f i x =
    assert_eq x (I.of_int i);
    assert_eq x (!r);
    r := I.add !r (I.of_int 1);
    I.sub x (I.of_int 1)
  in
  let b = A.mapi f a in
  check_i (A.sub b 1 499);

  let a = A.init 501 I.of_int in
  let r = ref (I.of_int 0) in
  let f i x =
    assert_eq x (I.of_int i);
    assert_eq x !r;
    r := I.add !r (I.of_int 1);
    I.sub x (I.of_int 1)
  in
  let b = A.mapi f a in
  check_i (A.sub b 1 500);

  (* [fold_left], test result and order of evaluation *)
  let a = A.init 500 I.of_int in
  let f acc x =
    assert_eq acc x;
    I.add x (I.of_int 1)
  in
  let acc = A.fold_left f (I.of_int 0) a in
  assert_eq acc (I.of_int 500);

  let a = A.init 501 I.of_int in
  let acc = A.fold_left f (I.of_int 0) a in
  assert_eq acc (I.of_int 501);

  (* [fold_right], test result and order of evaluation *)
  let a = A.init 500 I.of_int in
  let f x acc =
    assert_eq x (I.sub acc (I.of_int 1));
    x
  in
  let acc = A.fold_right f a (I.of_int 500) in
  assert_eq acc (I.of_int 0);

  let a = A.init 501 I.of_int in
  let acc = A.fold_right f a (I.of_int 501) in
  assert_eq acc (I.of_int 0);

  (* [iter2], test result and order of evaluation *)
  let a = A.init 123 I.of_int in
  let b = A.init 123 I.of_int in
  let r = ref (I.of_int 0) in
  let f x y =
    assert_eq x !r;
    assert_eq y !r;
    r := I.add!r (I.of_int 1);
  in
  A.iter2 f a b;
  let c = A.make 456 (I.of_int 0) in
  check_inval (A.iter2 (fun _ _ -> assert false) a) c;
  check_inval (A.iter2 (fun _ _ -> assert false) c) a;

  let a = A.init 124 I.of_int in
  let b = A.init 124 I.of_int in
  let r = ref (I.of_int 0) in
  let f x y =
    assert_eq x !r;
    assert_eq y !r;
    r := I.add !r (I.of_int 1);
  in
  A.iter2 f a b;

  (* [map2], test result and order of evaluation *)
  let a = A.init 456 I.of_int in
  let b = A.init 456 (fun i -> I.(mul (of_int i) (I.of_int 2))) in
  let r = ref (I.of_int 0) in
  let f x y =
    assert_eq x !r;
    assert_eq y (I.mul !r (I.of_int 2));
    r := I.add !r (I.of_int 1);
    I.(neg (sub x y))
  in
  let c = A.map2 f a b in
  check_i c;
  let d = A.make 455 (I.of_int 0) in
  check_inval (A.map2 (fun _ _ -> assert false) a) d;
  check_inval (A.map2 (fun _ _ -> assert false) d) a;

  let a = A.init 457 I.of_int in
  let b = A.init 457 (fun i -> I.(mul (of_int i) (I.of_int 2))) in
  let r = ref (I.of_int 0) in
  let f x y =
    assert_eq x !r;
    assert_eq y (I.mul !r (I.of_int 2));
    r := I.add !r (I.of_int 1);
    I.(neg (sub x y))
  in
  let c = A.map2 f a b in
  check_i c;

  (* [for_all], test result and order of evaluation *)
  let a = A.init 777 I.of_int in
  let r = ref (I.of_int 0) in
  let f x =
    assert_eq x !r;
    r := I.add x (I.of_int 1);
    true
  in
  assert (A.for_all f a);
  let f x = assert_eq x (I.of_int 0); false in
  assert (not (A.for_all f a));

  let a = A.init 778 I.of_int in
  let r = ref (I.of_int 0) in
  let f x =
    assert_eq x !r;
    r := I.add x (I.of_int 1);
    true
  in
  assert (A.for_all f a);
  let f x = assert_eq x (I.of_int 0); false in
  assert (not (A.for_all f a));

  (* [exists], test result and order of evaluation *)
  let a = A.init 777 I.of_int in
  let r = ref (I.of_int 0) in
  let f x =
    assert_eq x !r;
    r := I.add x (I.of_int 1);
    false
  in
  assert (not (A.exists f a));
  let f x = assert_eq x (I.of_int 0); true in
  assert (A.exists f a);

  let a = A.init 778 I.of_int in
  let r = ref (I.of_int 0) in
  let f x =
    assert_eq x !r;
    r := I.add x (I.of_int 1);
    false
  in
  assert (not (A.exists f a));
  let f x = assert_eq x (I.of_int 0); true in
  assert (A.exists f a);

  (* [mem] *)
  let a = A.init 7777 I.of_int in
  assert (A.mem (I.of_int 0) a);
  assert (A.mem (I.of_int 7776) a);
  assert (not (A.mem ((I.of_int (-1))) a));
  assert (not (A.mem (I.of_int 7777) a));
  let check v =
    A.set a 1000 v;
    assert (A.mem v a);
  in
  List.iter check [I.max_val; I.min_val; (I.of_int (-1)); (I.of_int 0)];

  let a = A.init 7778 I.of_int in
  assert (A.mem (I.of_int 0) a);
  assert (A.mem (I.of_int 7777) a);
  assert (not (A.mem ((I.of_int (-1))) a));
  assert (not (A.mem (I.of_int 7778) a));
  let check v =
    A.set a 1001 v;
    assert (A.mem v a);
  in
  List.iter check [I.max_val; I.min_val; (I.of_int (-1)); (I.of_int 0)];

  (* [find_opt], test result and order of evaluation *)
  let a = A.init 777 I.of_int in
  let r = ref (I.of_int 0) in
  let f x =
    assert_eq x !r;
    r := I.add x (I.of_int 1);
    false
  in
  assert (Option.is_none (A.find_opt f a));
  let f x = assert_eq x (I.of_int 0); true in
  assert (Option.is_some (A.find_opt f a));

  (* [find_index], test result and order of evaluation *)
  let a = A.init 777 I.of_int in
  let r = ref (I.of_int 0) in
  let f x =
    assert_eq x !r;
    r := I.add x (I.of_int 1);
    false
  in
  assert (Option.is_none (A.find_index f a));
  let f x = assert_eq x (I.of_int 0); true in
  assert (Option.get (A.find_index f a) = 0);

  (* [find_map], test result and order of evaluation *)
  let a = A.init 777 I.of_int in
  let r = ref (I.of_int 0) in
  let f x =
    assert_eq x !r;
    r := I.add x (I.of_int 1);
    None
  in
  assert (Option.is_none (A.find_map f a));
  let f x = assert_eq x (I.of_int 0); Some "abc" in
  assert (Option.get (A.find_map f a) = "abc");

  (* [find_mapi], test result and order of evaluation *)
  let a = A.init 777 I.of_int in
  let r = ref (I.of_int 0) in
  let r_i = ref 0 in
  let f i x =
    assert (i = !r_i);
    assert_eq x !r;
    r_i := !r_i + 1;
    r := I.add x (I.of_int 1);
    None
  in
  assert (Option.is_none (A.find_mapi f a));
  let f i x =
    assert (i = 0);
    assert_eq x (I.of_int 0);
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
  let rand_val _ =
    match Random.int 1000 with
    | n when n < 500 -> I.rand I.max_val
    | _ -> I.neg (I.rand I.max_val)
  in
  let check s =
    let a = A.init 5 I.of_int in
    check_sort s I.compare a; (* already sorted *)
    check_sort s (fun x y -> I.compare y x) a; (* reverse-sorted *)

    let a = A.init 6 I.of_int in
    check_sort s I.compare a; (* already sorted *)
    check_sort s (fun x y -> I.compare y x) a; (* reverse-sorted *)

    let a = A.of_list [I.max_val; I.min_val; (I.of_int (-1)); (I.of_int 0)] in
    check_sort s I.compare a; (* already sorted *)
    check_sort s (fun x y -> I.compare y x) a; (* reverse-sorted *)

    let a = A.init 50000 rand_val in
    check_sort s I.compare a;
    let a = A.init 50001 rand_val in
    check_sort s I.compare a;
    let a = A.make 1000 (I.of_int 1) in
    check_sort s I.compare a;
    let a = A.make 1001 (I.of_int 1) in
    check_sort s I.compare a;
    let a = A.append (A.make 1000 (I.of_int 1)) (A.make 1000 (I.of_int 2)) in
    check_sort s I.compare a;
    let a = A.append (A.make 1001 (I.of_int 1)) (A.make 1001 (I.of_int 2)) in
    check_sort s I.compare a;
  in
  check A.sort;
  check A.stable_sort;
  check A.fast_sort;

  (* [to_seq] *)
  let check_seq a =
    let r = ref 0 in
    let f x =
      assert_eq (A.get a !r) x;
      r := !r + 1;
    in
    let s = A.to_seq a in
    Seq.iter f s;
  in
  check_seq (A.init 999 I.of_int);
  check_seq (A.init 1000 I.of_int);
  check_seq (A.make 0 (I.of_int 0));

  (* [to_seqi] *)
  let check_seqi a =
    let r = ref 0 in
    let f (i, x) =
      assert (i = !r);
      assert_eq (A.get a !r) x;
      r := !r + 1;
    in
    let s = A.to_seqi a in
    Seq.iter f s;
  in
  check_seqi (A.init 999 I.of_int);
  check_seqi (A.init 1000 I.of_int);
  check_seqi (A.make 0 (I.of_int 0));

  (* [of_seq] *)
  let r = ref 0 in
  let rec f () =
    if !r = 100 then Seq.Nil else begin
      let res = Seq.Cons (I.of_int !r, f) in
      r := !r + 1;
      res
    end
  in
  let a = A.of_seq f in
  assert (A.equal a (A.init 100 I.of_int));
  assert (A.equal (A.of_seq Seq.empty) (A.make 0 (I.of_int 0)));

  (* [map_to_array] *)
  let r = ref 0 in
  let f x =
    assert_eq x (I.of_int !r);
    r := !r + 1;
    I.mul x (I.of_int 2)
  in
  let a = A.init 876 I.of_int in
  let ar1 = A.map_to_array f a in
  let ar2 = Array.init 876 (fun x -> I.of_int (2 * x)) in
  assert (Array.for_all2 (fun l r -> I.compare l r = 0) ar1 ar2);
  let ar = A.map_to_array (fun _ -> assert false) (A.make 0 (I.of_int 0)) in
  assert (ar = [| |]);

  (* [map_from_array] *)
  let r = ref 0 in
  let f x =
    assert_eq x (I.of_int !r);
    r := !r + 1;
    I.mul x (I.of_int 2)
  in
  let ar = Array.init 876 I.of_int in
  let a1 = A.map_from_array f ar in
  let a2 = A.init 876 (fun x -> I.of_int (2 * x)) in
  assert (A.equal a1 a2);
  let a = A.map_from_array (fun _ -> assert false) [| |] in
  assert (A.equal a (A.make 0 (I.of_int 0)));

  (* comparisons *)
  (* No polymorphic compare yet *)
  (* let normalize_comparison n =
    if n = 0 then 0 else if n < 0 then -1 else 1
  in
  let check c l1 l2 =
    assert (c = (normalize_comparison (compare (A.of_list l1) (A.of_list l2))))
  in
  check 0    [(I.of_int 0); (I.of_int 1); (I.of_int -4); I.max_val; I.min_val]
             [(I.of_int 0); (I.of_int 1); (I.of_int -4); I.max_val; I.min_val];
  check (-1) [(I.of_int 0); (I.of_int 1); (I.of_int -4); I.max_val; I.min_val]
             [(I.of_int 0); (I.of_int 1); (I.of_int -4); I.max_val; I.(add min_val (I.of_int 1))];
  check (-1) [(I.of_int 0); (I.of_int 1); (I.of_int -4); I.max_val; 4509684(I.of_int 3)]
             [(I.of_int 0); (I.of_int 1); (I.of_int -4); I.max_val; 4509684(I.of_int 4)];
  check 1    [(I.of_int 0); (I.of_int 2); (I.of_int -4)]
             [(I.of_int 0); (I.of_int 0); (I.of_int 3)];
  check 1    [(I.of_int 0); (I.of_int 2); (I.of_int -4)]
             [I.min_val; (I.of_int 0); (I.of_int 3)]; *)

  (* [unsafe_get] [unsafe_set] *)
  let a = A.make 3 (I.of_int 0) in
  for i = 0 to 2 do A.unsafe_set a i (I.of_int i) done;
  for i = 0 to 2 do assert_eq (A.unsafe_get a i) (I.of_int i) done;

  let a = A.make 4 (I.of_int 0) in
  for i = 0 to 3 do A.unsafe_set a i (I.of_int i) done;
  for i = 0 to 3 do assert_eq (A.unsafe_get a i) (I.of_int i) done;

  (* I/O *)
  (* No marshalling yet *)
  (* let test_structured_io value =
    let (tmp, oc) =
      Filename.open_temp_file ~mode:[Open_binary] "int64_array" ".data"
    in
    Marshal.to_channel oc value [];
    close_out oc;
    let ic = open_in_bin tmp in
    let value' = Marshal.from_channel ic in
    close_in ic;
    Sys.remove tmp;
    assert (compare value value' = 0)
  in
  let l = [(I.of_int 0); (I.of_int 1); (I.of_int -4); I.max_val; I.min_val; 3141592(I.of_int 6)] in
  test_structured_io (A.of_list l); *)

  (* map_inplace *)
  let a = A.init 4 (fun i -> I.of_int (i + 1)) in
  A.map_inplace (fun x -> I.mul x (I.of_int 2)) a;
  let got = A.map_to_array Fun.id a in
  let expected = [|I.of_int 2; I.of_int 4; I.of_int 6; I.of_int 8|] in
  assert (Array.for_all2 (fun x y -> I.compare x y = 0) got expected);

  (* mapi_inplace *)
  let a = A.init 4 (fun i -> I.of_int (i + 1)) in
  A.mapi_inplace (fun i x -> I.(add (add (of_int 1) (of_int i)) x)) a;
  let got = A.map_to_array Fun.id a in
  let expected = [|I.of_int 2; I.of_int 4; I.of_int 6; I.of_int 8|] in
  assert (Array.for_all2 (fun x y -> I.compare x y = 0) got expected);

  (* make_matrix *)
  check_inval (A.make_matrix (-1) 1) (I.of_int 1);
  check_inval (A.make_matrix 1 (-1)) (I.of_int 1);
  let check_matrix a =
    let row_len = Array.length a in
    assert (row_len > 0);
    let col_len = A.length (a.(0)) in
    for row = 0 to (row_len - 1) do
      assert (A.length (a.(row)) = col_len);
      for col = 0 to (col_len - 1) do
        assert (I.compare (A.get (a.(row)) col) (I.of_int 1) = 0)
      done
    done in
  let a = A.make_matrix 100 100 (I.of_int 1) in
  check_matrix a;
  let a = A.make_matrix 101 100 (I.of_int 1) in
  check_matrix a;
  let a = A.make_matrix 101 101 (I.of_int 1) in
  check_matrix a;
  let a = A.make_matrix 100 101 (I.of_int 1) in
  check_matrix a;

  (* for_all2 *)
  let test a =
    let r = ref 0 in
    let f x y =
      assert_eq x (I.of_int !r);
      assert_eq y (I.of_int !r);
      r := !r + 1;
      true
    in
    assert (A.for_all2 f a a);
    let f x y =
      assert_eq x (I.of_int 0); assert_eq y (I.of_int 0); false in
    assert (not (A.for_all2 f a a) = (A.length a > 0))
  in
  let a = A.init 777 I.of_int in
  test a;
  let a = A.init 778 I.of_int in
  test a;
  let a = A.init 0 I.of_int in
  test a;
  let a = A.init 778 I.of_int in
  let b = A.init 778 (fun _ -> I.of_int 0) in
  assert (A.for_all2 (fun a b -> I.compare a b >= 0) a b);
  assert (not (A.for_all2 (fun a b -> I.compare a b >= 0) b a));
  check_inval (fun x -> A.for_all2 (fun _ _ -> true) (A.make 100 x) (A.make 101 x))
    (I.of_int 1);

  (* exists2 *)
  let test a =
    let r = ref 0 in
    let f x y =
      assert_eq x (I.of_int !r);
      assert_eq y (I.of_int !r);
      r := !r + 1;
      false
    in
    assert (not (A.exists2 f a a));
    let f x y =
      assert_eq x (I.of_int 0); assert_eq y (I.of_int 0); true in
    assert (A.exists2 f a a = (A.length a > 0))

  in
  let a = A.init 777 I.of_int in
  test a;
  let a = A.init 778 I.of_int in
  test a;
  let a = A.init 0 I.of_int in
  test a;
  let a = A.init 778 I.of_int in
  let b = A.init 778 (fun _ -> I.of_int 0) in
  assert (A.exists2 (fun a b -> I.compare a b > 0) a b);
  assert (not (A.exists2 (fun a b -> I.compare a b > 0) b a));
  check_inval (fun x -> A.exists2 (fun _ _ -> true) (A.make 100 x) (A.make 101 x))
    (I.of_int 1);

end
