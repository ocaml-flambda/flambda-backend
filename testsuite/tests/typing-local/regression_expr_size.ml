(* TEST *)
let[@inline always] const () = 42

let[@inline always] alloc size =
  let local_ _blah = [size] in
  const ()

type t = { func : unit -> int; foo : int }

let[@inline never] mkrec () =
  let rec t = { func = (fun () -> t.foo); foo = alloc 50 } in
  t

let () = Printf.printf "%d\n" ((mkrec ()).func ())
