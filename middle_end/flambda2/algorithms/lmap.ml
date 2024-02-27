module type Thing = sig
  type t

  val equal : t -> t -> bool

  val print : Format.formatter -> t -> unit
end

module type S = sig
  type key

  type +'a t

  val empty : 'a t

  val is_empty : 'a t -> bool

  val add : key -> 'a -> 'a t -> 'a t

  val singleton : key -> 'a -> 'a t

  val disjoint_union : 'a t -> 'a t -> 'a t

  val disjoint_union_many : 'a t list -> 'a t

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val fold_left_map : ('a -> key -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t

  val filter : (key -> 'a -> bool) -> 'a t -> 'a t

  val keys : _ t -> key list

  val data : 'a t -> 'a list

  val bindings : 'a t -> (key * 'a) list

  val of_list : (key * 'a) list -> 'a t

  val find : key -> 'a t -> 'a

  val find_opt : key -> 'a t -> 'a option

  val get_singleton : 'a t -> (key * 'a) option

  val get_singleton_exn : 'a t -> key * 'a

  val map : ('a -> 'b) -> 'a t -> 'b t

  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t

  val map_sharing : ('a -> 'a) -> 'a t -> 'a t

  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t

  val to_seq : 'a t -> (key * 'a) Seq.t

  val for_all_with_fixed_arg : (key -> 'a -> 'b -> bool) -> 'a t -> 'b -> bool

  val exists : (key -> 'a -> bool) -> 'a t -> bool

  val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t

  val of_seq : (key * 'a) Seq.t -> 'a t

  val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val invariant : 'a t -> unit
end

module Make (T : Thing) : S with type key = T.t = struct
  type key = T.t

  type +'a t = (key * 'a) list

  let empty = []

  let is_empty m = match m with [] -> true | _ :: _ -> false

  let add k v m = (k, v) :: m

  let singleton k v = [k, v]

  let disjoint_union m1 m2 = m1 @ m2

  let disjoint_union_many ms = List.concat ms

  let iter f m = List.iter (fun (k, v) -> f k v) m

  let fold f m b = List.fold_left (fun b (k, v) -> f k v b) b m

  let fold_left_map f a m =
    List.fold_left_map
      (fun a (k, v) ->
        let a, c = f a k v in
        a, (k, c))
      a m

  let filter p m = List.filter (fun (k, v) -> p k v) m

  let rec for_all_with_fixed_arg f m fixed_arg =
    match m with
    | [] -> true
    | (k, v) :: m -> f k v fixed_arg && for_all_with_fixed_arg f m fixed_arg

  let exists f m = List.exists (fun (k, v) -> f k v) m

  let keys m = List.map fst m

  let data m = List.map snd m

  let bindings m = m

  let of_list m = m

  let find_opt k m =
    List.find_map (fun (k', v) -> if T.equal k k' then Some v else None) m

  let find k m = match find_opt k m with Some v -> v | None -> raise Not_found

  let get_singleton = function [(k, v)] -> Some (k, v) | _ -> None

  let get_singleton_exn = function [(k, v)] -> k, v | _ -> raise Not_found

  let map f m = List.map (fun (k, v) -> k, f v) m

  let mapi f m = List.map (fun (k, v) -> k, f k v) m

  let map_sharing f m =
    Misc.Stdlib.List.map_sharing
      (fun ((k, v) as pair) ->
        let v' = f v in
        if v' == v then pair else k, v')
      m

  let filter_map f m =
    List.filter_map (fun (k, v) -> f k v |> Option.map (fun v' -> k, v')) m

  let to_seq m = List.to_seq m

  let rec add_seq s m =
    match s () with Seq.Nil -> m | Seq.Cons (pair, s') -> pair :: add_seq s' m

  let of_seq m = List.of_seq m

  let print_assoc print_key print_datum ppf l =
    match l with
    | [] -> Format.fprintf ppf "{}"
    | _ :: _ ->
      Format.fprintf ppf "@[<hov 1>{%a}@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           (fun ppf (key, datum) ->
             Format.fprintf ppf "@[<hov 1>(%a@ %a)@]" print_key key print_datum
               datum))
        l

  let [@ocamlformat "disable"] print f fmt m = print_assoc T.print f fmt m

  let rec invariant m =
    match m with
    | [] -> ()
    | (k, _) :: m ->
      List.iter
        (fun (k', _) ->
          if T.equal k k' then Misc.fatal_errorf "Duplicate key: %a" T.print k)
        m;
      invariant m
end
