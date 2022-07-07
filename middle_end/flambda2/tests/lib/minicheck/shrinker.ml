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

let unshrinkable _ = Seq.empty

module Seq = struct
  include Seq

  let singleton a = Seq.cons a Seq.empty

  let rec concat ts =
    match ts with
    | [] -> Seq.empty
    | t :: ts -> Seq.append t (fun () -> concat ts ())
end

let rec list t l =
  match l with
  | [] -> Seq.empty
  | [a] -> Seq.cons [] (Seq.map (fun a -> [a]) (t a))
  | a :: l ->
    let empty = Seq.singleton [] in
    let drop_a = Seq.singleton l in
    let shrink_a = Seq.map (fun a -> a :: l) (t a) in
    let keep_a = Seq.map (fun l -> a :: l) (fun () -> list t l ()) in
    Seq.concat [empty; drop_a; shrink_a; keep_a]

let option t = function
  | None -> Seq.empty
  | Some a -> Seq.cons None (Seq.map Option.some (t a))

let function_ : type a b. ?const:b -> b t -> (a, b) Function.t t =
 fun ?const t function_ ->
  let shrink_as_const a = Seq.map (fun a -> Function.Const a) (t a) in
  match function_ with
  | Identity -> Seq.empty
  | Const a -> shrink_as_const a
  | Fun _ -> (
    match const with
    | None -> Seq.empty
    | Some const -> Seq.cons (Function.Const const) (shrink_as_const const))

let function_w_id : type a. ?const:a -> a t -> (a, a) Function.t t =
 fun ?const t c ->
  match c with
  | Identity -> Seq.empty
  | Const _ | Fun _ -> Seq.cons Function.Identity (function_ ?const t c)

let pair t_a t_b (a, b) =
  Seq.concat [Seq.map (fun a -> a, b) (t_a a); Seq.map (fun b -> a, b) (t_b b)]

let triple t_a t_b t_c (a, b, c) =
  Seq.concat
    [ Seq.map (fun a -> a, b, c) (t_a a);
      Seq.map (fun b -> a, b, c) (t_b b);
      Seq.map (fun c -> a, b, c) (t_c c) ]

let quad t_a t_b t_c t_d (a, b, c, d) =
  Seq.concat
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
    Seq.concat [shrink_a; shrink_tup]
  | _ :: _, [] | [], _ :: _ -> assert false
