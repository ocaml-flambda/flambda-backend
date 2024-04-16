let num_domains = 40

let create_domain () =
  let r = ref 0 in
  while true do
    (* non-allocating loop *)
    r := !r + Random.int 42
  done

let finaliser =
  let count = ref 0 in
  fun () ->
    Printf.printf "finaliser called (%d)\n%!" !count;
    incr count

let () =
  for i = 1 to num_domains do
    let (_ : _ Domain.t) = Domain.spawn create_domain in
    ()
  done;
  while true do
    for i = 1 to 1000 do
      let x = Sys.opaque_identity (Random.int 42, Random.int 42) in
      Gc.finalise_last finaliser x
    done;
    Gc.minor ()
  done

