(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Zesen Qian, Jane Street, London                     *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Mode
val transl_locality_mode_l : (allowed * 'r) Locality.t -> Lambda.locality_mode

val transl_alloc_mode_l : (allowed * 'r) Alloc.t -> Lambda.alloc_mode
val transl_alloc_mode_r : ('l * allowed) Alloc.t -> Lambda.alloc_mode

val transl_modify_mode : (allowed * 'r) Locality.t -> Lambda.modify_mode