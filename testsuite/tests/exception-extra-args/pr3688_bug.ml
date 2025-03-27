(* TEST *)

[@@@ocaml.flambda_o3]

let[@inline never] bar _ _ = ()

let[@inline never] baz () = ()

let[@inline] set arr x =
  Array.set arr 0 x

let foo size (x : int) arr =
  let extra_arg_array = ref arr in
  let extra_arg_int = ref 0 in
  (try
    for i = 0 to size - 1 do
      if i > 20 then (
        set !extra_arg_array x;
        baz ()
      )
    done
  with _exn -> ());
  bar !extra_arg_array !extra_arg_int
