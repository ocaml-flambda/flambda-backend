type 'a t = Format.formatter -> 'a -> unit

let str ppf s = Format.pp_print_string ppf s

let opaque_as s ppf _ = str ppf s

let opaque ppf a = opaque_as "?" ppf a

let bool = Format.pp_print_bool

let int = Format.pp_print_int

let option t ppf o =
  Format.pp_print_option ~none:(fun ppf () -> str ppf "<none>") t ppf o

let [@ocamlformat "disable"] list t ppf l =
  let pp_sep ppf () = Format.fprintf ppf "@,; " in
  Format.fprintf ppf "@[<hov>[ %a ]@]"
    (Format.pp_print_list ~pp_sep t) l

let unit ppf () = str ppf "()"

let fn ppf = opaque_as "<fun>" ppf

let [@ocamlformat "disable"] pair t_a t_b ppf (a, b) =
  Format.fprintf ppf "@[<hv>( %a@,, %a )@]"
    t_a a
    t_b b

let [@ocamlformat "disable"] triple t_a t_b t_c ppf (a, b, c) =
  Format.fprintf ppf "@[<hv>( %a@,, %a@,, %a )@]"
    t_a a
    t_b b
    t_c c

let [@ocamlformat "disable"] quad t_a t_b t_c t_d ppf (a, b, c, d) =
  Format.fprintf ppf "@[<hv>( %a@,, %a@,, %a@,, %a )@]"
    t_a a
    t_b b
    t_c c
    t_d d

module T = struct
  type nonrec 'a t = 'a t
end

let tuple : ('a, 'r) Tuple.Of(T).t -> ('a, 'r) Tuple.t t =
 fun ts ppf tup ->
  let rec loop :
      type a r.
      first:bool ->
      (a, r) Tuple.Of(T).t ->
      Format.formatter ->
      (a, r) Tuple.t ->
      unit =
   fun ~first tys ppf tup ->
    match tys, tup with
    | [], [] -> ()
    | t :: ts, a :: tup ->
      if not first then Format.fprintf ppf "@,, ";
      t ppf a;
      loop ~first:false ts ppf tup
    | _ :: _, [] | [], _ :: _ -> assert false
  in
  Format.fprintf ppf "@[<hv>( ";
  loop ~first:true ts ppf tup;
  Format.fprintf ppf " )@]"

let function_ : type a b. b t -> (a, b) Function.t t =
 fun t_ret ppf function_ ->
  match function_ with
  | Identity -> str ppf "(fun a -> a)"
  | Const b -> Format.fprintf ppf "@[<hov 2>(fun _ ->@ %a)@]" t_ret b
  | Fun f -> fn ppf f
