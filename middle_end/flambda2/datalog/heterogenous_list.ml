(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2024--2025 OCamlPro SAS                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type nil = Nil

module type S = sig
  type 'a t

  type _ hlist =
    | [] : nil hlist
    | ( :: ) : 'a t * 'b hlist -> ('a -> 'b) hlist
end

module Make (X : sig
  type 'a t
end) : S with type 'a t := 'a X.t = struct
  type 'a t = 'a X.t

  type _ hlist =
    | [] : nil hlist
    | ( :: ) : 'a t * 'b hlist -> ('a -> 'b) hlist
end

module Constant = Make (struct
  type 'a t = 'a
end)

module Option_ref = struct
  include Make (struct
    type 'a t = 'a option Named_ref.t
  end)

  let rec get : type s. s hlist -> s Constant.hlist = function
    | [] -> []
    | r :: rs -> Option.get r.contents :: get rs

  let rec set : type s. s hlist -> s Constant.hlist -> unit =
   fun refs values ->
    match refs, values with
    | [], [] -> ()
    | r :: rs, v :: vs ->
      r.contents <- Some v;
      set rs vs

  let rec pp_name_hlist : type s. _ -> s hlist -> unit =
   fun ff values ->
    match values with
    | [] -> ()
    | [x] -> Named_ref.pp_name ff x
    | x :: r -> Format.fprintf ff "%a, %a" Named_ref.pp_name x pp_name_hlist r
end
