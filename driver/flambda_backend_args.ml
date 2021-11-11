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

let mk_ocamlcfg f =
  "-ocamlcfg", Arg.Unit f, " Use ocamlcfg"

let mk_no_ocamlcfg f =
  "-no-ocamlcfg", Arg.Unit f, " Do not use ocamlcfg"
;;

module type Flambda_backend_options = sig
  val _ocamlcfg : unit -> unit
  val _no_ocamlcfg : unit -> unit
end

module Flambda_backend_options = struct
  let set r () = r := true
  let clear r () = r := false

  let _ocamlcfg = set Flambda_backend_flags.use_ocamlcfg
  let _no_ocamlcfg = clear Flambda_backend_flags.use_ocamlcfg
end

module Make_flambda_backend_options (F : Flambda_backend_options) =
struct
  let list2 = [
    mk_ocamlcfg F._ocamlcfg;
    mk_no_ocamlcfg F._no_ocamlcfg;
  ]
end

module Extra_params = struct
  let set name option =
    Some (fun ppf _position _name s ->
       Compenv.setter ppf (fun b -> b) name [ option ] s)

  let _clear name option =
    Some (fun ppf _position _name s ->
        Compenv.setter ppf (fun b -> not b) name [ option ] s)

  let read_param = function
    | "ocamlcfg" -> set "ocamlcfg" Flambda_backend_flags.use_ocamlcfg
    | _ -> None
end

module type Optcomp_options = sig
  include Main_args.Optcomp_options
  include Flambda_backend_options
end

module type Opttop_options = sig
  include Main_args.Opttop_options
  include Flambda_backend_options
end

module Make_optcomp_options (F : Optcomp_options) =
struct
  include Make_flambda_backend_options(F)
  include Main_args.Make_optcomp_options(F)
  let list = list2 @ list
end

module Make_opttop_options (F : Opttop_options) = struct
  include Make_flambda_backend_options(F)
  include Main_args.Make_opttop_options(F)
  let list = list2 @ list
end

module Default = struct
  module Optmain = struct
    include Main_args.Default.Optmain
    include Flambda_backend_options
  end
  module Opttopmain = struct
    include Main_args.Default.Opttopmain
    include Flambda_backend_options
  end
end
