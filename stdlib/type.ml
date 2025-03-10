(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

(* Type equality witness *)

type (_ : value_or_null, _ : value_or_null) eq = 
  Equal: ('a : value_or_null) . ('a , 'a) eq

(* Type identifiers *)

module Id = struct
  type (_ : value_or_null) id = ..
  module type ID = sig
    type t : value_or_null
    type _ id += Id : t id
  end

  type (!'a : value_or_null) t = (module ID with type t = 'a)

  let make (type a : value_or_null) () : a t =
    (module struct type t = a type _ id += Id : t id end)

  let[@inline] uid (type a : value_or_null) ((module A) : a t) =
    Obj.Extension_constructor.id (Obj.Extension_constructor.of_val A.Id)

  let provably_equal
      (type a : value_or_null) (type b : value_or_null)
      ((module A) : a t) ((module B) : b t) : (a, b) eq option
    =
    match A.Id with B.Id -> Some Equal | _ -> None
end
