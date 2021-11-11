(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*              Damien Doligez, projet Para, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
module type Flambda_backend_options = sig
  val _ocamlcfg : unit -> unit
  val _no_ocamlcfg : unit -> unit
end

module type Optcomp_options = sig
  include Main_args.Optcomp_options
  include Flambda_backend_options
end

module type Opttop_options = sig
  include Main_args.Opttop_options
  include Flambda_backend_options
end

module Make_optcomp_options : Optcomp_options -> Main_args.Arg_list;;
module Make_opttop_options : Opttop_options -> Main_args.Arg_list;;

module Default: sig
  module Optmain: Optcomp_options
  module Opttopmain: Opttop_options
end

module Extra_params : sig
  (* This is installed unconditionally for native and bytecode and
     all tools that consume OCAMLPARAM, to avoid warnings about flags
     that are only supported in flambda-backend,
     similarly to the treatment of native-only flags in
     [ocaml/driver/compenv.ml] *)
  val read_param : string ->
      (Format.formatter -> Compenv.readenv_position -> string -> string -> unit)
        option
end
