(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

include module type of struct
  include Int_ids.Variable
end

val create_with_same_name_as_ident : ?user_visible:unit -> Ident.t -> t

(** [rename] always returns a variable with a compilation unit set to that of
    the current unit, not the unit of the variable passed in. *)
val rename : ?append:string -> t -> t

val is_renamed_version_of : t -> t -> bool

val unique_name : t -> string

val raw_name : t -> string
