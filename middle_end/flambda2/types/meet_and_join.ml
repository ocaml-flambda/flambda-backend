(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let meet env t1 t2 =
  if Flambda_features.use_better_meet ()
  then Meet_and_join_new.meet env t1 t2
  else
    match Meet_and_join_old.meet (Typing_env.Meet_env.create env) t1 t2 with
    | Bottom -> Or_bottom.Bottom
    | Ok (ty, env_extension) ->
      Or_bottom.Ok
        ( ty,
          Typing_env.add_env_extension env env_extension
            ~meet_type:(Old Meet_and_join_old.meet) )

let[@inline] meet_type () =
  if Flambda_features.use_better_meet ()
  then Typing_env.New Meet_and_join_new.meet_type
  else Typing_env.Old Meet_and_join_old.meet

let meet_shape env t ~shape =
  if Flambda_features.use_better_meet ()
  then Meet_and_join_new.meet_shape env t ~shape
  else
    match Meet_and_join_old.meet_shape env t ~shape with
    | Bottom -> Or_bottom.Bottom
    | Ok env_extension ->
      Or_bottom.Ok
        (Typing_env.add_env_extension env env_extension
           ~meet_type:(Old Meet_and_join_old.meet))

let meet_env_extension env t1 t2 =
  if Flambda_features.use_better_meet ()
  then Meet_and_join_new.meet_env_extension env t1 t2
  else
    Meet_and_join_old.meet_env_extension (Typing_env.Meet_env.create env) t1 t2

let[@inline] join () =
  if Flambda_features.use_better_meet ()
  then Meet_and_join_new.join
  else Meet_and_join_old.join
