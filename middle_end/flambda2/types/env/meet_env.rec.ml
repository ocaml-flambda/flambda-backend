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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {
  env : Typing_env.t;
  already_meeting : Name.Pair.Set.t;
}

let [@ocamlformat "disable"] print ppf { env; already_meeting; } =
  Format.fprintf ppf
    "@[<hov 1>(\
      @[<hov 1>(env@ %a)@]@ \
      @[<hov 1>(already_meeting@ %a)@])@]"
    Typing_env.print env
    Name.Pair.Set.print already_meeting

let create env =
  { env;
    already_meeting = Name.Pair.Set.empty;
  }

let env t = t.env
      
let already_meeting_names t name1 name2 =
  Name.Pair.Set.mem (name1, name2) t.already_meeting
    || Name.Pair.Set.mem (name2, name1) t.already_meeting

let already_meeting t simple1 simple2 =
  let const _const = false in
  Simple.pattern_match simple1 ~const ~name:(fun name1 ~coercion:_ ->
    Simple.pattern_match simple2 ~const ~name:(fun name2 ~coercion:_ ->
      already_meeting_names t name1 name2))

let now_meeting_names t name1 name2 =
  if already_meeting_names t name1 name2 then begin
    Misc.fatal_errorf "Already meeting %a and %a:@ %a"
      Name.print name1
      Name.print name2
      print t
  end;
  let already_meeting =
    Name.Pair.Set.add (name1, name2) t.already_meeting
  in
  { t with
    already_meeting;
  }

let now_meeting t simple1 simple2 =
  let const _const = t in
  Simple.pattern_match simple1 ~const ~name:(fun name1 ~coercion:_ ->
    Simple.pattern_match simple2 ~const ~name:(fun name2 ~coercion:_ ->
      now_meeting_names t name1 name2))

(* let with_typing_env t typing_env =
 *   { t with
 *     env = typing_env;
 *   } *)
