(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Common subexpression elimination by value numbering over extended
   basic blocks. *)

open CSE_utils

class cse_generic : object
  (* The following methods can be overridden to handle processor-specific
     operations. *)

  method class_of_operation: Mach.operation -> op_class

  method is_cheap_operation: Mach.operation -> bool
    (* Operations that are so cheap that it isn't worth factoring them. *)

  (* The following method is the entry point and should not be overridden *)
  method fundecl: Mach.fundecl -> Mach.fundecl

end
