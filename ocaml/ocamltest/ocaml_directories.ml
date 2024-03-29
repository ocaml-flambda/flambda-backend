(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Locations of directories in the OCaml source tree *)

open Ocamltest_stdlib

let srcdir =
  Sys.getenv_with_default_value "OCAMLSRCDIR" Ocamltest_config.ocamlsrcdir

let stdlib =
  Filename.make_path [srcdir; "stdlib"]

let libunix =
  Filename.make_path [srcdir; "otherlibs"; "unix"]

let toplevel =
  Filename.make_path [srcdir; "toplevel"]

let runtime =
  let suffix = if Config.runtime5 then "" else "4" in
  Filename.make_path [srcdir; "runtime" ^ suffix]

let tools =
  Filename.make_path [srcdir; "tools"]
