[@@@ocaml.warning "+a-40-41-42"]

(* Dead code elimination: remove pure instructions whose results are not
   used. *)
val run : Cfg_with_infos.t -> Cfg_with_infos.t
