(* TEST *)

[@@@ocaml.flambda_o3]

let[@inline never] bar _ _ = ()

let[@inline never] baz () = ()

let foo size (x : int) arr y =
  let extra_arg_array = ref arr in
  let extra_arg_int = ref 0 in
  (try
    for i = 0 to size - 1 do
      if i > 20 then (
        let _ = Sys.opaque_identity (Obj.dup y) in
        baz ()
      )
    done
  with _exn -> ());
  bar !extra_arg_array !extra_arg_int
