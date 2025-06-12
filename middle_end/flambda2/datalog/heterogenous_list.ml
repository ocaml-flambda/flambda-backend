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
