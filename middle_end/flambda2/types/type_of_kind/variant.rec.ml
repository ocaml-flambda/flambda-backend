(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module T = Type_grammar

type t = {
  immediates : Type_grammar.t Or_unknown.t;
  blocks : Row_like.For_blocks.t Or_unknown.t;
  is_unique : bool;
}

(* CR mshinwell: This can now return [Or_bottom.t] *)
let create ~is_unique ~immediates ~blocks =
  begin match immediates with
  | Or_unknown.Unknown -> ()
  | Or_unknown.Known immediates ->
    if not (K.equal (T.kind immediates) K.naked_immediate) then begin
      Misc.fatal_errorf "Cannot create [immediates] with type that is not \
          of kind [Naked_immediate]:@ %a"
        T.print immediates
    end
  end;
  { immediates; blocks; is_unique; }
