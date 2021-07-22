let do_stuff env =
  assert false

let stuff env =
  (* r fails to be removed right now because the call to do_stuff
     unconditionnaly use its extra arg for data_flow *)
  let r = ref None in
  for i = 0 to 3 do
    try
      (Sys.opaque_identity do_stuff) env;
      r := Sys.opaque_identity (Some 12)
    with _ ->
      r := Sys.opaque_identity None;
      ()
  done;
