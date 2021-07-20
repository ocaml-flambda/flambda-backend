(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*            Mark Shinwell and Xavier Clerc, Jane Street Europe          *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = No_coeffects | Has_coeffects

let print ppf co =
  match co with
  | No_coeffects -> Format.fprintf ppf "no coeffects"
  | Has_coeffects -> Format.fprintf ppf "has coeffects"

let compare co1 co2 =
  match co1, co2 with
  | No_coeffects, No_coeffects -> 0
  | No_coeffects, Has_coeffects -> -1
  | Has_coeffects, Has_coeffects -> 0
  | Has_coeffects, No_coeffects -> 1

let join co1 co2 =
  match co1, co2 with
  | No_coeffects, No_coeffects -> No_coeffects
  | No_coeffects, Has_coeffects
  | Has_coeffects, Has_coeffects
  | Has_coeffects, No_coeffects -> Has_coeffects
