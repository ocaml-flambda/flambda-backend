# 2 "fun.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

[@@@ocaml.flambda_o3]

external id : ('a : value_or_null) . 'a -> 'a @@ portable = "%identity"
let const c _ = c
let compose f g x = f (g x)
let flip f x y = f y x
let negate p v = not (p v)

exception Finally_raised of exn

let () = Printexc.Safe.register_printer @@ function
| Finally_raised exn -> Some ("Fun.Finally_raised: " ^ Printexc.to_string exn)
| _ -> None

let protect ~(finally : unit -> unit) work =
  let finally_no_exn () =
    try finally () with e ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace (Finally_raised e) bt
  in
  match work () with
  | result -> finally_no_exn () ; result
  | exception work_exn ->
      let work_bt = Printexc.get_raw_backtrace () in
      finally_no_exn () ;
      Printexc.raise_with_backtrace work_exn work_bt
