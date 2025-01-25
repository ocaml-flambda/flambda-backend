(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type ('a,'b) t = ('a,'b) eval ref

and ('a,'b) eval =
  | Done of 'b
  | Raise of exn * Printexc.raw_backtrace
  | Thunk of 'a

type undo =
  | Nil
  | Cons : ('a, 'b) t * 'a * undo -> undo

type log = undo ref

let force f x =
  match !x with
  | Done x -> x
  | Raise (e, bt) -> Printexc.raise_with_backtrace e bt
  | Thunk e ->
      match f e with
      | y ->
        x := Done y;
        y
      | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        x := Raise (e, bt);
        raise e

let get_arg x =
  match !x with Thunk a -> Some a | _ -> None

let get_contents x =
  match !x with
  | Thunk a -> Either.Left a
  | Done b -> Either.Right b
  | Raise (e, bt) -> Printexc.raise_with_backtrace e bt

let create x =
  ref (Thunk x)

let create_forced y =
  ref (Done y)

let backtrace_size = 64

let create_failed e =
  let bt = Printexc.get_callstack backtrace_size in
  ref (Raise (e, bt))

let log () =
  ref Nil

let force_logged log f x =
  match !x with
  | Done x -> x
  | Raise (e, bt) -> Printexc.raise_with_backtrace e bt
  | Thunk e ->
    match f e with
    | (Error _ as err : _ result) ->
        x := Done err;
        log := Cons(x, e, !log);
        err
    | Ok _ as res ->
        x := Done res;
        res
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        x := Raise (e, bt);
        raise e

let backtrack log =
  let rec loop = function
    | Nil -> ()
    | Cons(x, e, rest) ->
        x := Thunk e;
        loop rest
  in
  loop !log
