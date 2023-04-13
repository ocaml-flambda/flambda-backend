(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Richard Eisenberg, Jane Street, New York                *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Format

(** Print some output to stdout, if [-debug-ocaml] is given on this
    invocation of ocaml. Example:

    {[
       Debug.print "The type is %a" Printtyp.raw_type_expr ty
    ]}
*)
val print : ('a, formatter, unit) format -> 'a
