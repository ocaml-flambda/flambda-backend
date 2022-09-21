type 'a t = Splittable_random.t -> 'a

let generate t = t

let map (t : 'a t) ~f : 'b t = fun r -> f (t r)

let bind (t : 'a t) ~f : 'b t = fun r -> f (t r) r

let filter ?(max_attempts = 1000) (t : 'a t) ~f : 'a t =
 fun r ->
  let rec loop i =
    if i < 1
    then
      failwith
        (Format.sprintf "filter: no successful value after %d attempts"
           max_attempts)
    else
      let a = t r in
      if f a then a else loop (i - 1)
  in
  loop max_attempts

module Let_syntax = struct
  let ( let+ ) t f = map t ~f

  let ( and+ ) t_a t_b : (_ * _) t =
   fun r ->
    let a = t_a r in
    let b = t_b r in
    a, b

  let ( let* ) t f = bind t ~f

  let ( and* ) = ( and+ )
end

open Let_syntax

let bool r = Splittable_random.int r mod 2 = 0

let int = Splittable_random.int

let small_nat ~less_than =
  if less_than <= 0
  then
    failwith
      (Format.sprintf "small_nat: upper bound %d must be positive" less_than)
  else
    let+ i = int in
    Int.abs i mod less_than

let log_int =
  let max_size =
    (* This assumes that the size of [int] is one less than the word size, which
       isn't true for JavaScript *)
    Sys.word_size - 1
  in
  fun r ->
    (* Pick a random size for our int, then pick an integer uniformly but only
       keep that many bits. (This doesn't produce an actual log-uniform
       distribution but it's good enough.) *)
    let size = Splittable_random.int r mod (max_size + 1) in
    if size = 0
    then 0
    else
      let high_bit = 1 lsl (size - 1) in
      (* Effectively, we've already decided that the [size]th bit is 1, so it
         only remains to pick uniformly a number of size [size-1] *)
      let mask = high_bit - 1 in
      let other_bits = Splittable_random.int r land mask in
      high_bit lor other_bits

let option t : _ option t =
 fun r ->
  let choose_none = bool r in
  if choose_none then None else Some (t r)

let list t ~length : _ list t =
  if length < 0
  then failwith (Format.sprintf "list: length must be non-negative: %d" length)
  else
    fun r ->
    let rec loop n acc = if n = 0 then acc else loop (n - 1) (t r :: acc) in
    loop length []

let fn ?(hash_arg = Hashtbl.hash) t_ret r =
  let base = Splittable_random.split r in
  fun a ->
    let r = Splittable_random.copy base in
    Splittable_random.perturb r (hash_arg a);
    t_ret r

let fn2 ?hash_args ret_ty =
  fn ?hash_arg:hash_args ret_ty |> map ~f:(fun f a b -> f (a, b))

let fn3 ?hash_args ret_ty =
  fn ?hash_arg:hash_args ret_ty |> map ~f:(fun f a b c -> f (a, b, c))

let const a _r = a

let one_of l =
  let () =
    match l with [] -> failwith "Generator.one_of: empty list" | _ :: _ -> ()
  in
  let+ i = small_nat ~less_than:(List.length l) in
  List.nth l i

let choose (choices : (int * 'a t) list) : _ t =
  let sum = List.fold_left (fun sum (w, _) -> sum + w) 0 choices in
  fun r ->
    let rec choose_generator i choices =
      match choices with
      | [] -> failwith "no choices"
      | (w, gen) :: _ when i < w -> gen
      | (w, _) :: choices -> choose_generator (i - w) choices
    in
    let i = small_nat ~less_than:sum r in
    let generator = choose_generator i choices in
    generator r

let function_ ?hash_arg t_ret : (_, _) Function.t t =
  choose
    [ ( 1,
        let+ r = t_ret in
        Function.Const r );
      ( 3,
        let+ f = fn ?hash_arg t_ret in
        Function.Fun f ) ]

let function_w_id ?hash_arg t_ret : (_, _) Function.t t =
  choose [1, const Function.Identity; 4, function_ ?hash_arg t_ret]

let unit = const ()

let pair t_a t_b : (_ * _) t =
 fun r ->
  let a = t_a r in
  let b = t_b r in
  a, b

let triple t_a t_b t_c : (_ * _ * _) t =
 fun r ->
  let a = t_a r in
  let b = t_b r in
  let c = t_c r in
  a, b, c

let quad t_a t_b t_c t_d : (_ * _ * _ * _) t =
 fun r ->
  let a = t_a r in
  let b = t_b r in
  let c = t_c r in
  let d = t_d r in
  a, b, c, d

module T = struct
  type nonrec 'a t = 'a t
end

let rec tuple : type a r. (a, r) Tuple.Of(T).t -> (a, r) Tuple.t t =
 fun ts r ->
  match ts with
  | [] -> []
  | t :: ts ->
    let a = t r in
    a :: tuple ts r
