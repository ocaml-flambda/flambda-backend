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

type 'a t = 'a -> 'a Seq.t

let atomic _ = Seq.empty

module Seq = struct
  include Seq

  let singleton a = Seq.cons a Seq.empty

  let round_robin (type a) ts =
    let open struct
      type state =
        { next_round_rev : a t list;
          this_round : a t list
        }
    end in
    let rec step { next_round_rev; this_round } =
      match this_round with
      | [] -> (
        match next_round_rev with
        | [] -> None
        | _ ->
          step { next_round_rev = []; this_round = List.rev next_round_rev })
      | seq :: this_round -> (
        match (seq () : a Seq.node) with
        | Nil -> step { next_round_rev; this_round }
        | Cons (a, seq) ->
          let next_round_rev = seq :: next_round_rev in
          Some (a, { next_round_rev; this_round }))
    in
    Seq.unfold step { next_round_rev = []; this_round = ts }
end

let rec list t l =
  match l with
  | [] -> Seq.empty
  | [a] -> Seq.cons [] (Seq.map (fun a -> [a]) (t a))
  | a :: l ->
    let drop_a = Seq.singleton l in
    let shrink_a = Seq.map (fun a -> a :: l) (t a) in
    let keep_a = Seq.map (fun l -> a :: l) (fun () -> list t l ()) in
    Seq.cons [] (Seq.round_robin [drop_a; shrink_a; keep_a])

let option t = function
  | None -> Seq.empty
  | Some a -> Seq.cons None (Seq.map Option.some (t a))

let code : type a b. ?const:b -> b t -> (a, b) Code.t t =
 fun ?const t code ->
  let shrink_as_const a = Seq.map (fun a -> Code.Const a) (t a) in
  match code with
  | Identity -> Seq.empty
  | Const a -> shrink_as_const a
  | Fun _ -> (
    match const with
    | None -> Seq.empty
    | Some const -> Seq.cons (Code.Const const) (shrink_as_const const))

let code_w_id : type a. ?const:a -> a t -> (a, a) Code.t t =
 fun ?const t c ->
  match c with
  | Identity -> Seq.empty
  | Const _ | Fun _ -> Seq.cons Code.Identity (code ?const t c)

let pair t_a t_b (a, b) =
  Seq.round_robin
    [Seq.map (fun a -> a, b) (t_a a); Seq.map (fun b -> a, b) (t_b b)]

let triple t_a t_b t_c (a, b, c) =
  Seq.round_robin
    [ Seq.map (fun a -> a, b, c) (t_a a);
      Seq.map (fun b -> a, b, c) (t_b b);
      Seq.map (fun c -> a, b, c) (t_c c) ]

let quad t_a t_b t_c t_d (a, b, c, d) =
  Seq.round_robin
    [ Seq.map (fun a -> a, b, c, d) (t_a a);
      Seq.map (fun b -> a, b, c, d) (t_b b);
      Seq.map (fun c -> a, b, c, d) (t_c c);
      Seq.map (fun d -> a, b, c, d) (t_d d) ]

module T = struct
  type nonrec 'a t = 'a t
end

let rec tuple :
    type a r. (a, r) Tuple.Of(T).t -> (a, r) Tuple.t -> (a, r) Tuple.t Seq.t =
 fun ts tup ->
  match ts, tup with
  | [], [] -> Seq.empty
  | t :: ts, a :: tup ->
    let shrink_a = Seq.map (fun a -> Tuple.cons a tup) (t a) in
    let shrink_tup =
      Seq.map (fun tup -> Tuple.cons a tup) (fun () -> tuple ts tup ())
    in
    Seq.round_robin [shrink_a; shrink_tup]
  | _ :: _, [] | [], _ :: _ -> assert false
