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

type t =
  {
    central_env : Meet_env.t;
    left_join_env : Typing_env.t;
    right_join_env : Typing_env.t;
  }

let _print ppf { central_env; left_join_env; right_join_env; } =
    let join_env name ppf env =
      Format.fprintf ppf "@ @[<hov 1>(%s@ %a)@]@" name
        Typing_env.print env
    in
    Format.fprintf ppf
      "@[<hov 1>(\
       @[<hov 1>(central_env@ %a)@]%a%a)@]"
      Meet_env.print central_env
      (join_env "left_join_env") left_join_env
      (join_env "right_join_env") right_join_env

let create central_env ~left_env ~right_env =
  { central_env = Meet_env.create central_env;
    left_join_env = left_env;
    right_join_env = right_env;
  }

(* let create_for_meet central_env =
 *   let typing_env = Meet_env.env central_env in
 *   { central_env = central_env;
 *     left_join_env = typing_env;
 *     right_join_env = typing_env;
 *   } *)

let target_join_env t =
  Meet_env.env t.central_env

let left_join_env t = t.left_join_env

let right_join_env t = t.right_join_env

(* CR mshinwell: fix naming, it's odd at the moment to be using
   [already_meeting]... *)
let now_joining t simple1 simple2 =
  { t with central_env = Meet_env.now_meeting t.central_env simple1 simple2;
  }

let already_joining { central_env; _ } simple1 simple2 =
  Meet_env.already_meeting central_env simple1 simple2
