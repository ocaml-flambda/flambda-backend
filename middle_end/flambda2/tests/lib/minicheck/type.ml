(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2022 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type 'a t =
  { generate : Splittable_random.t -> 'a;
    print : Format.formatter -> 'a -> unit
  }

let generate t = t.generate

let print t = t.print

let print_unknown ppf _ = Format.pp_print_string ppf "_"

let define ~generate ?(print = print_unknown) () = { generate; print }

let with_print t ~print = { t with print }

let bool =
  let generate r = Splittable_random.int r mod 2 = 0 in

  define () ~generate ~print:Format.pp_print_bool

let int = define () ~generate:Splittable_random.int ~print:Format.pp_print_int

let map_generate ty ~f =
  let generate r = f (generate ty r) in
  define () ~generate ~print:(print ty)

(* Should be small because we're doing this the dumb way *)
let small_nat ~less_than =
  map_generate int ~f:(fun i -> Int.abs i mod less_than)

let log_int =
  let generate r =
    (* This assumes that the size of [int] is one less than the word size, which
       isn't true for JavaScript *)
    let bits = Splittable_random.int r mod Sys.word_size in
    if bits = 0
    then 0
    else
      let high_bit = 1 lsl (bits - 1) in
      let mask = high_bit - 1 in
      Splittable_random.int r land mask lor high_bit
  in
  define () ~generate ~print:Format.pp_print_int

let option ty =
  let generate r =
    let choose_none = generate bool r in
    if choose_none then None else Some (generate ty r)
  in
  let print ppf o =
    Format.pp_print_option
      ~none:(fun ppf () -> Format.pp_print_string ppf "<none>")
      (print ty) ppf o
  in
  define () ~generate ~print

let map { generate; print = _ } ~f =
  let generate r = f (generate r) in
  define () ~generate

let bind { generate; print = _ } ~f =
  let generate r =
    let a = generate r in
    let { generate; print = _ } = f a in
    generate r
  in
  define () ~generate

(* Would love to make these print functions local, but doing so crashes
   ocamlformat due to the [@ocamlformat "disable"]. See
   https://github.com/ocaml-ppx/ocamlformat/issues/2096 *)

let [@ocamlformat "disable"] print_list ty ppf l =
  let pp_sep ppf () = Format.fprintf ppf "@,, " in
  Format.fprintf ppf "@[<hov>[ %a ]@]"
    (Format.pp_print_list ~pp_sep (print ty)) l

let list ty ~expected_length =
  let generate r =
    let rec loop acc =
      let stop = Splittable_random.int r mod (expected_length + 1) = 0 in
      if stop
      then acc
      else
        let a = generate ty r in
        loop (a :: acc)
    in
    loop []
  in
  let print = print_list ty in
  define ~generate ~print ()

let print_fn ppf _ = Format.pp_print_string ppf "fun"

let fn ?(hash_arg = Hashtbl.hash) ret_ty =
  let generate r =
    let base = Splittable_random.split r in
    fun a ->
      let r = Splittable_random.copy base in
      Splittable_random.perturb r (hash_arg a);
      generate ret_ty r
  in
  define ~generate ~print:print_fn ()

let fn2 ?hash_args ret_ty =
  fn ?hash_arg:hash_args ret_ty
  |> map ~f:(fun f a b -> f (a, b))
  |> with_print ~print:print_fn

let fn3 ?hash_args ret_ty =
  fn ?hash_arg:hash_args ret_ty
  |> map ~f:(fun f a b c -> f (a, b, c))
  |> with_print ~print:print_fn

let const a =
  let generate _r = a in
  define () ~generate

let one_of l =
  small_nat ~less_than:(List.length l) |> map ~f:(fun i -> List.nth l i)

let choose choices =
  let sum = List.fold_left (fun sum (w, _) -> sum + w) 0 choices in
  let generate r =
    let rec choose_generator i choices =
      match choices with
      | [] -> failwith "no choices"
      | (w, gen) :: _ when i < w -> gen
      | (w, _) :: choices -> choose_generator (i - w) choices
    in
    let i = generate (small_nat ~less_than:sum) r in
    let generator = choose_generator i choices in
    generate generator r
  in
  let print =
    (* Just pick whatever prints the first element?

       CR lmaurer: Yuck. This persuades me that maybe coupling generators and
       printers together was a bad idea. *)
    match choices with
    | [] -> failwith "no choices"
    | (_, { print; _ }) :: _ -> print
  in
  define () ~generate ~print

let unit =
  let generate _ = () in
  let print ppf () = Format.pp_print_string ppf "()" in
  define () ~generate ~print

let [@ocamlformat "disable"] print_pair ty1 ty2 ppf (a, b) =
  Format.fprintf ppf "@[<hv>( %a@,, %a )@]"
  (print ty1) a
  (print ty2) b

let pair ty1 ty2 =
  let generate r =
    let a = generate ty1 r in
    let b = generate ty2 r in
    a, b
  in
  let print = print_pair ty1 ty2 in
  define () ~generate ~print

let [@ocamlformat "disable"] print_triple ty1 ty2 ty3 ppf (a, b, c) =
  Format.fprintf ppf "@[<hv>( %a@,, %a@,, %a )@]"
    (print ty1) a
    (print ty2) b
    (print ty3) c

let triple ty1 ty2 ty3 =
  let generate r =
    let a = generate ty1 r in
    let b = generate ty2 r in
    let c = generate ty3 r in
    a, b, c
  in
  let print = print_triple ty1 ty2 ty3 in
  define () ~generate ~print

let [@ocamlformat "disable"] print_quad ty1 ty2 ty3 ty4 ppf (a, b, c, d) =
  Format.fprintf ppf
    "@[<hv>( %a@,, %a@,, %a@,, %a )@]"
    (print ty1) a
    (print ty2) b
    (print ty3) c
    (print ty4) d

let quad ty1 ty2 ty3 ty4 =
  let generate r =
    let a = generate ty1 r in
    let b = generate ty2 r in
    let c = generate ty3 r in
    let d = generate ty4 r in
    a, b, c, d
  in
  let print = print_quad ty1 ty2 ty3 ty4 in
  define () ~generate ~print

module Tuple = struct
  type 'a type_ = 'a t

  type ('a, 'b) t =
    | [] : ('a, 'a) t
    | ( :: ) : 'a type_ * ('b, 'c) t -> ('a -> 'b, 'c) t

  module Value = struct
    type ('a, 'b) tuple = ('a, 'b) t

    type ('a, 'b) t =
      | [] : ('a, 'a) t
      | ( :: ) : 'a * ('b, 'c) t -> ('a -> 'b, 'c) t

    let rec generate : type a b. (a, b) tuple -> Splittable_random.t -> (a, b) t
        =
     fun tys r ->
      match tys with
      | [] -> []
      | ty :: tys ->
        let a = ty.generate r in
        a :: generate tys r

    let print : ('a, 'b) tuple -> Format.formatter -> ('a, 'b) t -> unit =
     fun tys ppf t ->
      let rec loop :
          type a b.
          first:bool -> (a, b) tuple -> Format.formatter -> (a, b) t -> unit =
       fun ~first tys ppf t ->
        match tys, t with
        | [], [] -> ()
        | ty :: tys, a :: t ->
          if not first then Format.fprintf ppf "@,, ";
          ty.print ppf a;
          loop ~first:false tys ppf t
        | _, _ -> assert false
      in
      Format.fprintf ppf "@[<hv>( ";
      loop ~first:true tys ppf t;
      Format.fprintf ppf " )@]"
  end
end

let tuple tys =
  define () ~generate:(Tuple.Value.generate tys) ~print:(Tuple.Value.print tys)

module Let_syntax = struct
  let ( let+ ) a f = map a ~f

  let ( and+ ) = pair

  let ( let* ) a f = bind a ~f

  let ( and* ) = pair
end
